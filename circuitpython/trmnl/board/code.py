import os, io, time, alarm, board, wifi, socketpool, ssl, displayio, supervisor
import digitalio, neopixel, terminalio
import adafruit_requests, adafruit_imageload

SERVER = os.getenv("BYOS_URL")
CACHE = "/last.bmp"                 # image currently on screen, for the stale badge
RETRY_MIN, RETRY_MAX = 300, 3600    # failure backoff: 5 min, doubling, up to 1 h

# Buttons in physical order, LEFT to RIGHT. Page 1 is the leftmost button.
# If presses pick the wrong page, reverse this tuple.
BUTTONS = (board.BUTTON_D, board.BUTTON_C, board.BUTTON_B, board.BUTTON_A)

# sleep_memory layout
#   [0] filename length, [1:33] filename of the image on screen
F_FAILS = 40        # consecutive failed updates
F_STALE = 41        # 1 = stale badge is on screen
F_PAGE = 42         # page on screen (0-based)
F_COUNT = 43        # number of pages in the last server response (0 = unknown)
F_DEADLINE = 44     # [44:48] time.time() of the scheduled wake
F_BUTTON = 48       # button pressed during "hold" mode, +1 (0 = none)
F_CACHED = 49       # 1 = CACHE matches what's on screen
MEM_USED = 64

mem = alarm.sleep_memory
if (alarm.wake_alarm is None
        and supervisor.runtime.run_reason == supervisor.RunReason.STARTUP):
    mem[0:MEM_USED] = bytes(MEM_USED)   # cold boot or reset button: start over

stage = "start"


# ---------- small helpers ----------

def get_last():
    n = mem[0]
    return bytes(mem[1:1 + n]).decode() if 0 < n <= 32 else ""


def set_last(name):
    b = name.encode()[:32]
    mem[0] = len(b)
    mem[1:1 + len(b)] = b


def buttons_in():
    ins = []
    for p in BUTTONS:
        b = digitalio.DigitalInOut(p)
        b.switch_to_input(pull=digitalio.Pull.UP)
        ins.append(b)
    return ins


def wait_release(limit=10):
    """Don't sleep while a button is held, or it would wake us right back up."""
    ins = buttons_in()
    end = time.monotonic() + limit
    while time.monotonic() < end and not all(b.value for b in ins):
        time.sleep(0.05)
    for b in ins:
        b.deinit()


def wake_button():
    """Index of the button that woke us, or None."""
    b = mem[F_BUTTON]
    if b:                                      # pressed during "hold" mode
        mem[F_BUTTON] = 0
        return b - 1
    a = alarm.wake_alarm
    if isinstance(a, alarm.pin.PinAlarm):
        for i, p in enumerate(BUTTONS):
            if a.pin == p:
                return i
    return None


def remaining_sleep():
    """Seconds left until the scheduled wake, or None if unknown."""
    deadline = int.from_bytes(bytes(mem[F_DEADLINE:F_DEADLINE + 4]), "little")
    left = deadline - time.time()
    return left if 0 < left <= 86400 else None


def go_to_sleep(seconds):
    seconds = max(10, int(seconds))
    mem[F_DEADLINE:F_DEADLINE + 4] = (int(time.time()) + seconds).to_bytes(4, "little")
    print("sleeping", seconds, "s")
    wait_release()
    timer = alarm.time.TimeAlarm(monotonic_time=time.monotonic() + seconds)
    try:
        pins = [alarm.pin.PinAlarm(pin=p, value=False, pull=True) for p in BUTTONS]
        alarm.exit_and_deep_sleep_until_alarms(timer, *pins)
    except Exception as e:                     # never let button setup stop us sleeping
        print("button wake unavailable:", repr(e))
        alarm.exit_and_deep_sleep_until_alarms(timer)


def parse_color(c):
    # accepts "#RRGGBB", "RRGGBB", or an int
    if isinstance(c, int):
        return c
    return int(c.lstrip("#"), 16)


def start_leds(spec):
    """Light the NeoPixels per spec; returns pixels object or None."""
    colors = spec.get("colors")
    if not colors:
        return None
    pwr = digitalio.DigitalInOut(board.NEOPIXEL_POWER)
    pwr.switch_to_output(value=False)          # active low: False = power on
    px = neopixel.NeoPixel(board.NEOPIXEL, 4,
                           brightness=float(spec.get("brightness", 0.1)),
                           auto_write=False)
    if len(colors) == 1:
        colors = colors * 4
    for i, c in enumerate(colors[:4]):
        px[i] = parse_color(c)
    px.show()
    return px


# ---------- display ----------

def show(group):
    d = board.DISPLAY
    d.root_group = group
    time.sleep(d.time_to_refresh)
    d.refresh()
    for _ in range(100):                       # let the refresh finish before sleeping
        if not getattr(d, "busy", False):
            break
        time.sleep(0.1)


def image_group(src):
    bmp, pal = adafruit_imageload.load(src, bitmap=displayio.Bitmap,
                                       palette=displayio.Palette)
    g = displayio.Group()
    g.append(displayio.TileGrid(bmp, pixel_shader=pal))
    return g


def cache(data):
    """Save the image on screen. False if the drive isn't writable (USB mode)."""
    try:
        with open("/last.tmp", "wb") as f:
            f.write(data)
        try:
            os.remove(CACHE)
        except OSError:
            pass
        os.rename("/last.tmp", CACHE)
        return True
    except OSError as e:
        print("not caching image:", e)
        return False


def draw_stale(reason):
    """Redraw the on-screen image with an 'offline' badge. False if we can't."""
    if not mem[F_CACHED]:
        return False                           # cache missing or not what's on screen
    from adafruit_display_text import label
    g = image_group(CACHE)
    d = board.DISPLAY
    g.append(label.Label(terminalio.FONT, text="offline: " + reason,
                         color=0xFFFFFF, background_color=0x000000,
                         padding_left=3, padding_right=3,
                         padding_top=1, padding_bottom=1,
                         anchor_point=(1.0, 1.0),
                         anchored_position=(d.width, d.height)))
    show(g)
    return True


# ---------- update ----------

def run(want_page):
    """One update showing want_page if it exists. Returns seconds to sleep."""
    global stage
    stage = "wifi"
    wifi.radio.connect(os.getenv("CIRCUITPY_WIFI_SSID"),
                       os.getenv("CIRCUITPY_WIFI_PASSWORD"), timeout=15)

    pool = socketpool.SocketPool(wifi.radio)
    http = adafruit_requests.Session(pool, ssl.create_default_context())
    mac = ":".join(f"{b:02X}" for b in wifi.radio.mac_address)
    ap = wifi.radio.ap_info
    rssi = str(ap.rssi) if ap else "0"

    stage = "server"
    resp = http.get(f"{SERVER}/api/display", timeout=15,
                    headers={"ID": mac, "Access-Token": os.getenv("BYOS_KEY"),
                             "RSSI": rssi})
    if resp.status_code != 200:
        raise RuntimeError(f"HTTP {resp.status_code}")
    r = resp.json()

    # "pages" is optional; without it the top-level image is the only page
    pages = r.get("pages") or [{"image_url": r["image_url"], "filename": r["filename"]}]
    pages = pages[:len(BUTTONS)]
    mem[F_COUNT] = len(pages)
    page = want_page if want_page < len(pages) else 0

    refresh = int(r.get("refresh_rate", 1800))
    led_spec = r.get("leds") or {}
    mode = led_spec.get("mode", "flash")
    t_on = time.monotonic()
    try:
        px = start_leds(led_spec)              # LEDs are optional; never fail the update
    except Exception as e:
        print("leds:", e)
        px = None

    # redraw for a new image, a page change, or to clear a stale badge
    p = pages[page]
    if p["filename"] != get_last() or mem[F_STALE]:
        stage = "image"
        data = http.get(p["image_url"], timeout=30).content
        show(image_group(io.BytesIO(data)))
        set_last(p["filename"])
        mem[F_CACHED] = 1 if cache(data) else 0
    mem[F_PAGE] = page
    mem[F_FAILS] = 0
    mem[F_STALE] = 0

    if px and mode == "hold":
        # Stay awake with LEDs on until the next refresh. USB power only.
        # Buttons still switch pages: note which one and restart.
        wifi.radio.enabled = False
        ins = buttons_in()
        end = time.monotonic() + max(1, refresh - (time.monotonic() - t_on))
        while time.monotonic() < end:
            for i, b in enumerate(ins):
                if not b.value and i < len(pages) and i != page:
                    mem[F_BUTTON] = i + 1
                    supervisor.reload()
            time.sleep(0.05)
        supervisor.reload()

    if px:
        remaining = float(led_spec.get("seconds", 5)) - (time.monotonic() - t_on)
        if remaining > 0:
            time.sleep(remaining)

    return refresh


def on_failure():
    n = min(mem[F_FAILS] + 1, 255)
    mem[F_FAILS] = n
    if not mem[F_STALE]:                       # badge only once per outage
        try:
            if draw_stale(stage):
                mem[F_STALE] = 1
        except Exception as e:
            print("stale badge failed:", repr(e))
    return min(RETRY_MIN * 2 ** (n - 1), RETRY_MAX)


# ---------- main ----------

try:
    button = wake_button()
    want = mem[F_PAGE]
    if button is not None:
        print("woke by button", button + 1, "of", len(BUTTONS))
        known = mem[F_COUNT]
        if known and (button >= known or button == want):
            left = remaining_sleep()
            if left:                           # no such page, or already shown:
                go_to_sleep(left)              # back to sleep, no network
        else:
            want = button
    sleep_for = run(want)
except Exception as e:                         # anything at all: don't crash, just sleep
    print(f"update failed ({stage}):", repr(e))
    try:
        sleep_for = on_failure()
    except Exception as e2:
        print("failure handling failed:", repr(e2))
        sleep_for = RETRY_MAX

go_to_sleep(sleep_for)
