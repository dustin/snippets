from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import keypad
import microcontroller
import socketpool
import time
import traceback
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode, WatchDogTimeout
import circuitpython_schedule as schedule
import neopixel

import busio
import board

pixel_circle_pin = board.D10
num_circle_pixels = 12

pm25 = None
scd = None

try:
    i2c = busio.I2C(board.SCL, board.SDA, frequency=100000)
except:
    i2c = None

try:
    from adafruit_pm25.i2c import PM25_I2C
    if i2c:
        pm25 = PM25_I2C(i2c)
except:
    pm25 = None

try:
    import adafruit_scd30
    if i2c:
        scd = adafruit_scd30.SCD30(i2c)
        scd.measurement_interval = 10
        scd.self_calibration_enabled = True
except:
    print("Failed to initialize SCD-30")
    scd = None

try:
    from secrets import secrets
except ImportError:
    print("WiFi secrets are kept in secrets.py, please add them there!")
    raise

AQI_TOPIC="home/purpleair/aqi"
TIME_TOPIC="home/local/time"
ACTIVITY_TOPIC="weather/kihei/activity"
WIND_TOPIC="weather/kihei/status"
WINDD_TOPIC="weather/kihei/direction"
NETSTATE_TOPIC="home/ping/8.8.8.8/label"
VOLT_TOPIC="home/magtag/{mqtt_username}/voltage".format(**secrets)
BAT_TOPIC="home/magtag/{mqtt_username}/battery".format(**secrets)
PM25_TOPIC="home/magtag/{mqtt_username}/pm2.5".format(**secrets)
CO2_TOPIC="home/magtag/{mqtt_username}/co2".format(**secrets)
TEMP_TOPIC="home/magtag/{mqtt_username}/temperature".format(**secrets)
HUMIDITY_TOPIC="home/magtag/{mqtt_username}/humidity".format(**secrets)
DISPLAY_TOPIC="home/magtag/{mqtt_username}/display".format(**secrets)
BUTTON_TOPIC="home/magtag/{mqtt_username}/button/".format(**secrets)
INFO_TOPIC="home/magtag/{mqtt_username}/info".format(**secrets)
DOORBELL_TOPIC="home/doorbell/ding"
PW_STATE_TOPIC="home/power/batteryState"

DOORBELL_SECS=30

# EPA PM2.5 AQI breakpoints (2024 revision):
# (conc low, conc high, index low, index high), concentration in ug/m3.
PM25_BREAKPOINTS = [
    (0.0, 9.0, 0, 50),
    (9.1, 35.4, 51, 100),
    (35.5, 55.4, 101, 150),
    (55.5, 125.4, 151, 200),
    (125.5, 225.4, 201, 300),
    (225.5, 325.4, 301, 500),
]

def pm25ToAQI(c):
    c = int(c * 10) / 10  # EPA truncates to one decimal
    for clo, chi, ilo, ihi in PM25_BREAKPOINTS:
        if c <= chi:
            return round((ihi - ilo) / (chi - clo) * (c - clo) + ilo)
    return 500

print("Reset reason:", microcontroller.cpu.reset_reason)

# RAISE so a stall produces a traceback showing where we were stuck.
# The handler at the bottom resets the board afterwards, so we still get
# a clean restart.
w.timeout=60.0
w.mode = WatchDogMode.RAISE
w.feed()

magtag = MagTag()

# Scan the buttons in the background so presses aren't missed while we're
# blocked in MQTT, and so each press is reported once rather than for as
# long as it's held.  The MagTag library already owns the pins.
BUTTON_NAMES = ['a', 'b', 'c', 'd']
for b in magtag.peripherals.buttons:
    b.deinit()
buttons = keypad.Keys(
    (board.BUTTON_A, board.BUTTON_B, board.BUTTON_C, board.BUTTON_D),
    value_when_pressed=False, pull=True)

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

magtag.peripherals.neopixels.fill((0, 0, 0))
magtag.peripherals.neopixel_disable = False

pixel_circle = neopixel.NeoPixel(pixel_circle_pin, num_circle_pixels, brightness=0.1, auto_write=False)

class State:
    def __init__(self):
        self.dirty = False
        self.time = None
        self.aqiIn = None
        self.aqiOut = None
        self.co2 = None
        self.netState = 'ok'
        self.activity = 'bad'
        self.wind = 'unkn'
        self.windDir = None
        self.volts = None
        self.display = False
        self.canRedraw = True
        self.ledColors = [(0,0,0),(0,0,0),(0,0,0),(0,0,0)]
        self.blinkState = [False, False, False, False]
        self.doorbellJob = None

        pool = socketpool.SocketPool(wifi.radio)

        self.mqtt_client = MQTT.MQTT(
            broker=secrets["broker"],
            port=secrets["port"],
            username=secrets["mqtt_username"],
            password=secrets["mqtt_pw"],
            socket_pool=pool,
        )

    def enableDisplay(self):
        self.display = True
        self.dirty = True
        self.drawRing()

    def disableDisplay(self):
        self.display = False
        magtag.peripherals.neopixels.fill((0,0,0))
        pixel_circle.fill((0, 0, 0))
        pixel_circle.show()

    def gotDisplay(self, client, topic, msg):
        print("got display", msg)
        if msg == 'on':
            self.enableDisplay()
        else:
            self.disableDisplay()

    def gotTime(self, client, topic, t):
        self.time = t

    def gotAQI(self, client, topic, a):
        print("got AQI", a)
        try:
            v = float(a)
        except (ValueError, TypeError):
            print("Invalid AQI value:", a)
            return
        if self.aqiOut is None or round(v) != round(self.aqiOut):
            self.dirty = True
        self.aqiOut = v

    def updatePM25(self):
        if not pm25:
            return
        try:
            aqdata = pm25.read()
        except RuntimeError as e:
            print("PM2.5 read failed:", e)
            return
        # See also "pm10 standard", "pm100 standard", "pm10 env", "pm25 env", "pm100 env"
        pm = aqdata["pm25 standard"]
        self.mqtt_client.publish(PM25_TOPIC, pm, retain=True)
        self.mqtt_client.loop()
        aqi = pm25ToAQI(pm)
        if aqi != self.aqiIn:
            self.aqiIn = aqi
            self.dirty = True

    def updateCO2(self):
        if not scd:
            return
        try:
            if not scd.data_available:
                return
            co2 = scd.CO2
            temp = scd.temperature
            rh = scd.relative_humidity
        except (OSError, RuntimeError) as e:
            print("SCD-30 read failed:", e)
            return

        if co2 < 300 or co2 > 10000:
            print("Invalid co2 reading of", co2)
            return

        if self.co2 is None or round(co2) != round(self.co2):
            self.dirty = True
        self.co2 = co2

        self.mqtt_client.publish(CO2_TOPIC, co2, retain=True)
        self.mqtt_client.publish(TEMP_TOPIC, temp, retain=True)
        self.mqtt_client.publish(HUMIDITY_TOPIC, rh, retain=True)
        self.mqtt_client.loop()
        print("read co2", self.co2)

    def updateBattery(self):
        self.volts = magtag.peripherals.battery
        self.mqtt_client.publish(VOLT_TOPIC, self.volts, retain=True)
        self.mqtt_client.publish(BAT_TOPIC, min(100, self.volts*100 / 4.2), retain=True)
        self.mqtt_client.loop()

    def gotNetState(self, client, topic, t):
        print("got net state", t)
        self.netState = t
        colors = {'ok': (0, 8, 0),
                  'slow': (127, 63, 0),
                  'loss': (127, 0, 0)}
        self.ledColors[0] = colors.get(t, (0, 0, 0))

    def gotActivity(self, client, topic, t):
        print("got activity", t)
        self.activity = t
        colors = {'bad': (0, 0, 0),
                  'paddleboarding': (51, 102, 0),
                  'winging': (127, 25, 127)}
        self.ledColors[2] = colors.get(t, (0, 0, 0))

    def gotWind(self, client, topic, t):
        if self.wind != t:
            print("got wind", t)
            self.wind = t
            self.dirty = True
            self.drawRing()

    def gotWindDir(self, client, topic, t):
        try:
            d = int(float(t))
            print("got wind direction", t)
        except (ValueError, TypeError):
            print("Invalid wind direction:", t)
            return
        if self.windDir != d:
            self.windDir = d
            self.drawRing()

    def drawRing(self):
        if not self.display or self.windDir is None:
            return

        color = (0, 5, 0)
        try:
            mag = int(self.wind.split('g')[1])
            brightness = min(255, int(pow(mag / 40, 1.5) * 255))

            color = (
                min(brightness // 5, 50),
                min(brightness // 25, 10),
                min(brightness // 5, 50)
            )
        except (ValueError, IndexError):
            pass

        pixel_circle.fill((0, 0, 0))
        pixel_circle[(self.windDir % 360) // 30] = color
        pixel_circle.show()

    def allowRedraw(self):
        self.canRedraw = True

    def readyToDraw(self):
        return self.canRedraw and self.time is not None and self.windDir is not None

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

        aqis = []
        if self.aqiIn is not None:
            aqis.append('In: {inside:.0f}'.format(inside=self.aqiIn))
        if self.aqiOut is not None:
            aqis.append('Out: {outside:.0f}'.format(outside=self.aqiOut))
        magtag.set_text('AQI ' + (', '.join(aqis)), 2, False)
        if self.co2 is not None:
            magtag.set_text('CO2: {co2:.0f} ppm'.format(co2=self.co2), 3, False)
        magtag.set_text(self.wind, 0, False)
        magtag.set_text('{time}                  {windDir}°'.format(time=self.time, windDir=self.windDir), 1, False)
        w.feed()
        try:
            display.refresh()
        except RuntimeError as e:
            print("Refresh failed, will retry:", e)
            return
        w.feed()
        self.mqtt_client.publish(INFO_TOPIC, 'drawing')
        self.dirty = False
        self.canRedraw = False

    def blink(self, n, color):
        if self.blinkState[n]:
            self.ledColors[n] = (0, 0, 0)
        else:
            self.ledColors[n] = color
        self.blinkState[n] = not self.blinkState[n]

    def gotDoorbell(self, client, topic, t):
        if t != 'on': return

        # A new ding restarts the blinking.
        if self.doorbellJob:
            schedule.cancel_job(self.doorbellJob)

        end = time.monotonic() + DOORBELL_SECS

        def blinkDoorbell():
            if time.monotonic() >= end:
                self.blinkState[1] = False
                self.ledColors[1] = (0, 0, 0)
                self.doorbellJob = None
                return schedule.CancelJob
            self.blink(1, (127, 0, 0))

        self.doorbellJob = schedule.every(0.5).seconds.do(blinkDoorbell)

    def gotPWState(self, client, topic, t):
        print("got powerwall state")
        self.ledColors[3] = (0, 0, 8) if t == 'charged' else (8, 0, 0)

    def mqtt_loop(self):
        if self.mqtt_client:
            self.mqtt_client.loop()

state = State()
schedule.every(60).seconds.do(state.updatePM25)
schedule.every(10).seconds.do(state.updateCO2)
schedule.every(60).seconds.do(state.updateBattery)
schedule.every(60).seconds.do(state.allowRedraw)
schedule.every(1).seconds.do(state.mqtt_loop)
schedule.every(1).seconds.do(w.feed)

def init():
    pixel_circle.fill((0, 0, 0))
    pixel_circle.show()

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

    # 1: Time and Direction
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, magtag.graphics.display.height - 14),
    )

    # 2: AQI
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, 2),
        text_anchor_point=(0, 0)
    )

    # 3: CO2
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

    state.mqtt_client.add_topic_callback(AQI_TOPIC, state.gotAQI)
    state.mqtt_client.add_topic_callback(TIME_TOPIC, state.gotTime)
    state.mqtt_client.add_topic_callback(NETSTATE_TOPIC, state.gotNetState)
    state.mqtt_client.add_topic_callback(ACTIVITY_TOPIC, state.gotActivity)
    state.mqtt_client.add_topic_callback(WIND_TOPIC, state.gotWind)
    state.mqtt_client.add_topic_callback(WINDD_TOPIC, state.gotWindDir)
    state.mqtt_client.add_topic_callback(DISPLAY_TOPIC, state.gotDisplay)
    state.mqtt_client.add_topic_callback(DOORBELL_TOPIC, state.gotDoorbell)
    state.mqtt_client.add_topic_callback(PW_STATE_TOPIC, state.gotPWState)
    print("Connecting to MQTT: ", secrets["broker"])
    state.mqtt_client.connect()
    w.feed()
    print("Subscribing to a bunch of junk")
    state.mqtt_client.subscribe(AQI_TOPIC)
    state.mqtt_client.subscribe(TIME_TOPIC)
    state.mqtt_client.subscribe(NETSTATE_TOPIC)
    state.mqtt_client.subscribe(DISPLAY_TOPIC)
    state.mqtt_client.subscribe(DOORBELL_TOPIC)
    state.mqtt_client.subscribe(PW_STATE_TOPIC)
    state.mqtt_client.subscribe(ACTIVITY_TOPIC)
    state.mqtt_client.subscribe(WIND_TOPIC)
    state.mqtt_client.subscribe(WINDD_TOPIC)

def handleButtons():
    pressed = False
    event = buttons.events.get()
    while event:
        if event.pressed:
            state.mqtt_client.publish(BUTTON_TOPIC + BUTTON_NAMES[event.key_number], 1, qos=1)
            pressed = True
        event = buttons.events.get()
    if pressed:
        state.mqtt_client.loop()

def main():
    w.feed()
    init()

    magtag.peripherals.neopixels.fill((0, 0, 0))
    schedule.run_all()

    while True:
        handleButtons()
        schedule.run_pending()
        state.draw()
        time.sleep(0.05)

try:
    main()
except (Exception, WatchDogTimeout) as e:
    print("oh no: exception")
    traceback.print_exception(e, e, e.__traceback__)
    cylon((16,0,0))
# Restarting from scratch is what reliably recovers (fresh WiFi, fresh MQTT
# client, fresh display state), so do that instead of retrying in place.
time.sleep(5)
microcontroller.reset()
