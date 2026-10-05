import os, io, time, alarm, board, wifi, socketpool, ssl, displayio, supervisor
import digitalio, neopixel
import adafruit_requests, adafruit_imageload

SERVER = os.getenv("BYOS_URL")
pool = socketpool.SocketPool(wifi.radio)
http = adafruit_requests.Session(pool, ssl.create_default_context())
mac = ":".join(f"{b:02X}" for b in wifi.radio.mac_address)

mem = alarm.sleep_memory

def get_last():
    n = mem[0]
    return bytes(mem[1:1 + n]).decode() if 0 < n <= 32 else ""

def set_last(name):
    b = name.encode()[:32]
    mem[0] = len(b)
    mem[1:1 + len(b)] = b

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

wifi.radio.connect(os.getenv("CIRCUITPY_WIFI_SSID"), os.getenv("CIRCUITPY_WIFI_PASSWORD"))

ap = wifi.radio.ap_info
rssi = str(ap.rssi) if ap else "0"

r = http.get(f"{SERVER}/api/display",
             headers={"ID": mac, "Access-Token": os.getenv("BYOS_KEY"),
                      "RSSI": rssi}).json()

refresh = int(r.get("refresh_rate", 900))
led_spec = r.get("leds") or {}
mode = led_spec.get("mode", "flash")
t_on = time.monotonic()
px = start_leds(led_spec)                      # light up early so the e-ink refresh overlaps

last = get_last()
if r["filename"] != last:
    bmp, pal = adafruit_imageload.load(io.BytesIO(http.get(r["image_url"]).content),
                                       bitmap=displayio.Bitmap, palette=displayio.Palette)
    g = displayio.Group(); g.append(displayio.TileGrid(bmp, pixel_shader=pal))
    board.DISPLAY.root_group = g
    time.sleep(board.DISPLAY.time_to_refresh)
    board.DISPLAY.refresh()
    set_last(r["filename"])

if px and mode == "hold":
    # Stay awake with LEDs on until the next refresh. USB power only.
    wifi.radio.enabled = False
    time.sleep(max(1, refresh - (time.monotonic() - t_on)))
    supervisor.reload()

if px:
    remaining = float(led_spec.get("seconds", 5)) - (time.monotonic() - t_on)
    if remaining > 0:
        time.sleep(remaining)

alarm.exit_and_deep_sleep_until_alarms(
    alarm.time.TimeAlarm(monotonic_time=time.monotonic() + refresh))