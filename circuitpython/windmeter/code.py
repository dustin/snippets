from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import board
import keypad
import microcontroller
import socketpool
import time
import traceback
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode
import circuitpython_schedule as schedule
import json

try:
    from secrets import secrets
except ImportError:
    print("WiFi secrets are kept in secrets.py, please add them there!")
    raise

TIME_TOPIC="home/local/time"
PERIOD_TOPIC="home/magtag/period"
VOLT_TOPIC="home/magtag/{mqtt_username}/voltage".format(**secrets)
BAT_TOPIC="home/magtag/{mqtt_username}/battery".format(**secrets)
DUR_TOPIC="home/magtag/{mqtt_username}/duration".format(**secrets)
WIND_TOPIC="weather/+"
MIN_LIGHT=500
DEFAULT_STATION='kihei'
# If the saved station hasn't reported after this long, fall back to another.
STATION_GRACE=30

# Selected station is kept in NVM so it survives resets and deep sleep.
# Layout: magic byte, length byte, utf-8 station name.
NVM_MAGIC=0xA5
NVM_MAX=64

sleepTime=900

# RESET rather than RAISE: a raised WatchDogTimeout can land anywhere
# (including the middle of an e-ink refresh) and leave things wedged.
# A real reset is what fixes it by hand, so let the watchdog do that.
w.timeout=20.0
w.mode = WatchDogMode.RESET
w.feed()

magtag = MagTag()

# Scan button C in the background so presses aren't missed while we're
# blocked in MQTT or a display refresh.  The MagTag library already owns
# the pin, so release it first.
magtag.peripherals.buttons[2].deinit()
buttons = keypad.Keys((board.BUTTON_C,), value_when_pressed=False, pull=True)

def loadStation():
    nvm = microcontroller.nvm
    try:
        if nvm[0] == NVM_MAGIC and 0 < nvm[1] <= NVM_MAX:
            return bytes(nvm[2:2 + nvm[1]]).decode('utf-8')
    except Exception as e:
        print("Couldn't load station:", e)
    return DEFAULT_STATION

def saveStation(name):
    b = name.encode('utf-8')[:NVM_MAX]
    microcontroller.nvm[0:2 + len(b)] = bytes([NVM_MAGIC, len(b)]) + b
    print("Saved station", name)

def cylon(color):
    w.feed()
    magtag.peripherals.neopixels.fill((0, 0, 0))
    magtag.peripherals.neopixel_disable = False
    for i in [3, 2, 1, 0, 1, 2, 3]:
        magtag.peripherals.neopixels[i] = color
        time.sleep(0.1)
        magtag.peripherals.neopixels[i] = (0, 0, 0)
        magtag.peripherals.neopixels.show()
    magtag.peripherals.neopixel_disable = True
    w.feed()


# Before we do anything of interest, check to see if the light's on.
# If it's dark, we shouldn't do anything.
magtag.peripherals.neopixels.fill((0, 0, 0))
magtag.peripherals.neopixel_disable = False
if magtag.peripherals.light < MIN_LIGHT:
    print("Light is {0}, guess I'll sleep now".format(magtag.peripherals.light))
    cylon((4,0,0))
    magtag.exit_and_deep_sleep(60)


class State:
    def __init__(self):
        self.dirty = False
        self.time = None
        self.windVals = {}
        self.wind = 'unkn'
        self.current = loadStation()
        self.saved = self.current
        self.stations = [self.current]
        self.canRedraw = True
        self.display = True
        self.ledColors = [(0, 0, 0)] * 4
        self.volts = None
        self.started = None

        pool = socketpool.SocketPool(wifi.radio)

        self.mqtt_client = MQTT.MQTT(
            broker=secrets["broker"],
            port=secrets["port"],
            username=secrets["mqtt_username"],
            password=secrets["mqtt_pw"],
            socket_pool=pool,
        )

    def mqtt_loop(self):
        if self.mqtt_client:
            self.mqtt_client.loop()

    def readyToDraw(self):
        return (self.canRedraw and self.time is not None
                and self.volts is not None and self.current in self.windVals)

    def draw(self):
        if not self.display:
            return
        for i in range(4):
            magtag.peripherals.neopixels[i] = self.ledColors[i]
        magtag.peripherals.neopixels.show()

        if not (self.dirty and self.readyToDraw()):
            return

        # Never block waiting on the e-ink.  If it's not ready yet, stay
        # dirty and try again on a later pass through the main loop.
        display = magtag.graphics.display
        if display.time_to_refresh > 0 or display.busy:
            return

        wv = self.windVals[self.current]
        print("Drawing ", wv['label'])
        magtag.set_text(wv['label'], 2, False)
        magtag.set_text(self.wind, 0, False)
        magtag.set_text('{dir_card} {dir_deg}°'.format(**wv), 3, False)
        magtag.set_text('{time}                 {bat:.2f}V'.format(time=self.time, bat=self.volts), 1, False)
        w.feed()
        try:
            display.refresh()
        except RuntimeError as e:
            print("Refresh failed, will retry:", e)
            return
        w.feed()
        self.dirty = False
        self.canRedraw = False

        # Persist only once a selection has actually been shown, so quickly
        # cycling through stations doesn't write NVM for each one.
        if self.current != self.saved:
            saveStation(self.current)
            self.saved = self.current

    def forceDraw(self):
        self.dirty = True
        self.canRedraw = True
        self.updateWindText()

    def updateWindText(self):
        if self.current not in self.windVals:
            return
        w = self.windVals[self.current]
        d = '{avg}g{gust}'.format(avg=round(w['avg']), gust=round(w['gust']))
        if d != self.wind:
            self.wind = d
            self.dirty = True

    def gotWind(self, client, topic, t):
        try:
            w = json.loads(t)
            label = w['shortLabel']
        except (ValueError, KeyError, TypeError) as e:
            print("Bad wind message on", topic, e)
            return
        self.windVals[label] = w
        if label not in self.stations:
            self.stations.append(label)
            self.stations = sorted(self.stations)
            print("Stations now:", self.stations)

        if label == self.current:
            self.updateWindText()

    def allowRedraw(self):
        self.canRedraw = True

    def gotTime(self, client, topic, t):
        self.time = t

    def updateBattery(self):
        self.volts = magtag.peripherals.battery
        self.mqtt_client.publish(VOLT_TOPIC, self.volts, retain=True)
        self.mqtt_client.publish(BAT_TOPIC, min(100, self.volts*100 / 4.2), retain=True)
        self.mqtt_client.loop()

    def advance(self, n):
        c = self.stations.index(self.current)
        self.current = self.stations[(c + n) % len(self.stations)]
        print("Changed to station ", self.current)
        self.wind = 'unkn'
        self.forceDraw()

    def checkStation(self):
        # The saved station may no longer be published.  Rather than sit on
        # a blank screen forever, drop it and show something that exists.
        if self.started is None or self.current in self.windVals:
            return
        if not self.windVals or time.monotonic() - self.started < STATION_GRACE:
            return
        print("No data for", self.current, "- falling back")
        self.stations.remove(self.current)
        self.current = self.stations[0]
        self.saved = self.current  # don't overwrite the user's choice
        self.forceDraw()

state = State()
schedule.every(1).seconds.do(w.feed)
schedule.every(1).seconds.do(state.mqtt_loop)
schedule.every(1).seconds.do(state.checkStation)
schedule.every(60).seconds.do(state.updateBattery)
schedule.every(60).seconds.do(state.allowRedraw)

def main():
    w.feed()

    # 0: Big display
    magtag.add_text(
        # text_font="/fonts/Helvetica-Bold-100.bdf",
        text_font="/fonts/Poetsen-60.bdf",
        text_position=(
            (magtag.graphics.display.width // 2) - 1,
            (magtag.graphics.display.height // 2) - 20,
        ),
        text_anchor_point=(0.5, 0.5),
    )

    # 1: Time and Voltage
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, magtag.graphics.display.height - 14),
    )

    # 2: Location Label
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, 2),
        text_anchor_point=(0, 0)
    )

    # 3: Wind Direction
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(magtag.graphics.display.width - 6, 2),
        text_anchor_point=(1, 0)
    )

    w.feed()
    print("Available WiFi networks:")
    for network in wifi.radio.start_scanning_networks():
        print("\t%s\t\tRSSI: %d\tChannel: %d" % (str(network.ssid, "utf-8"),
                network.rssi, network.channel))
    wifi.radio.stop_scanning_networks()
    w.feed()
    magtag.peripherals.neopixel_disable = False
    magtag.peripherals.neopixels.fill((8, 0, 0))
    print("Connecting to ", secrets["ssid"])
    wifi.radio.connect(secrets["ssid"], secrets["password"])
    magtag.peripherals.neopixels.fill((6, 3, 16))

    w.feed()

    state.mqtt_client.add_topic_callback(TIME_TOPIC, state.gotTime)
    state.mqtt_client.add_topic_callback(WIND_TOPIC, state.gotWind)
    state.mqtt_client.connect()
    state.mqtt_client.subscribe(TIME_TOPIC)
    state.mqtt_client.subscribe(WIND_TOPIC)
    state.started = time.monotonic()

    w.feed()

    magtag.peripherals.neopixels.fill((0, 0, 0))
    schedule.run_all()

    while True:
        presses = 0
        event = buttons.events.get()
        while event:
            if event.pressed:
                presses += 1
            event = buttons.events.get()
        if presses:
            state.advance(presses)

        schedule.run_pending()
        state.draw()
        time.sleep(0.05)

try:
    main()
except Exception as e:
    print("oh no: exception")
    traceback.print_exception(e, e, e.__traceback__)
    cylon((16,0,0))
# Restarting from scratch is what reliably recovers (fresh WiFi, fresh MQTT
# client, fresh display state), so do that instead of retrying in place.
# The selected station survives in NVM.
time.sleep(5)
microcontroller.reset()
