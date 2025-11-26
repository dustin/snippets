from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import time
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode
import time
import circuitpython_schedule as schedule
from adafruit_datetime import timedelta
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
PERIOD_TOPIC="home/magtag/period"
VOLT_TOPIC="home/magtag/{mqtt_username}/voltage".format(**secrets)
BAT_TOPIC="home/magtag/{mqtt_username}/battery".format(**secrets)
PM25_TOPIC="home/magtag/{mqtt_username}/pm2.5".format(**secrets)
CO2_TOPIC="home/magtag/{mqtt_username}/co2".format(**secrets)
TEMP_TOPIC="home/magtag/{mqtt_username}/temperature".format(**secrets)
HUMIDITY_TOPIC="home/magtag/{mqtt_username}/humidity".format(**secrets)
DUR_TOPIC="home/magtag/{mqtt_username}/duration".format(**secrets)
DISPLAY_TOPIC="home/magtag/{mqtt_username}/display".format(**secrets)
BUTTON_TOPIC="home/magtag/{mqtt_username}/button/".format(**secrets)
INFO_TOPIC="home/magtag/{mqtt_username}/info".format(**secrets)
DOORBELL_TOPIC="home/doorbell/ding"
PW_STATE_TOPIC="home/power/batteryState"

MIN_LIGHT=500

sleepTime=900

w.timeout=60.0
w.mode = WatchDogMode.RAISE
w.feed()

magtag = MagTag()

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

pixel_circle = neopixel.NeoPixel(pixel_circle_pin, num_circle_pixels, brightness=0.3, auto_write=False)

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
        self.windDir = 0
        self.volts = None
        self.display = False
        self.canRedraw = True
        self.ledColors = [(0,0,0),(0,0,0),(0,0,0),(0,0,0)]
        self.blinkState = [False, False, False, False]

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

    def disableDisplay(self):
        self.display = False
        magtag.peripherals.neopixels.fill((0,0,0))

    def gotDisplay(self, client, topic, msg):
        print("got display", msg)
        if msg == 'on':
            self.enableDisplay()
        else:
            self.disableDisplay()

    def gotTime(self, client, topic, t):
        # print("Got time", t)
        self.time = t

    def gotAQI(self, client, topic, a):
        print("got AQI", a)
        try:
            self.aqiOut = float(a)
            self.dirty = True
        except (ValueError, TypeError):
            print("Invalid AQI value:", a)

    def updatePM25(self):
        if pm25:
            aqdata = pm25.read()
            # See also "pm10 standard", "pm100 standard", "pm10 env", "pm25 env", "pm100 env"
            self.mqtt_client.publish(PM25_TOPIC, aqdata["pm25 standard"], retain=True)
            self.mqtt_client.loop()
            self.aqiIn = aqdata["pm25 standard"]
            self.dirty = True

    def updateCO2(self):
        if not (scd and scd.data_available):
            print("SCD 30 unavailable")
            return

        co2 = scd.CO2
        temp = scd.temperature
        rh = scd.relative_humidity

        if co2 < 300 or co2 > 10000:
            print("Invalid co2 reading of", co2)
            return

        self.co2 = co2

        self.mqtt_client.publish(CO2_TOPIC, co2, retain=True)
        self.mqtt_client.publish(TEMP_TOPIC, temp, retain=True)
        self.mqtt_client.publish(HUMIDITY_TOPIC, rh, retain=True)
        self.mqtt_client.loop()
        print("read co2", self.co2)
        self.dirty = True

    def updateBattery(self):
        self.volts = magtag.peripherals.battery
        self.mqtt_client.publish(VOLT_TOPIC, self.volts, retain=True)
        self.mqtt_client.publish(BAT_TOPIC, min(100, self.volts*100 / 4.2), retain=True)
        self.mqtt_client.loop()

    def gotNetState(self, client, topic, t):
        print("got net state")
        self.netState = t
        colors = {'ok': (0, 8, 0),
                  'slow': (127, 63, 0),
                  'loss': (127, 0, 0)}
        self.ledColors[0] = colors[t]

    def gotActivity(self, client, topic, t):
        print("got activity")
        self.activity = t  # Fix variable name
        colors = {'bad': (0, 0, 0),
                  'paddleboarding': (51, 102, 0),
                  'winging': (127, 25, 127)}
        self.ledColors[2] = colors[t]

    def gotWind(self, client, topic, t):
        if self.wind != t:
            self.wind = t
            self.dirty = True

    def gotWindDir(self, client, topic, t):
        if self.windDir != int(t):
            self.windDir = int(t)

            color = (0, 5, 0)
            try:
                mag = int(self.wind.split('g')[1])
                color = (5 * mag, 1 * mag, 5 * mag)
            except:
                pass

            pixel_circle.fill((0, 0, 0))
            pixel_circle[int(min(359, self.windDir) / 30)] = color
            pixel_circle.show()

    def allowRedraw(self):
        self.canRedraw = True

    def readyToDraw(self):
        return self.canRedraw and self.time is not None and self.volts is not None

    def draw(self):
        if self.display:
            for i in range(4):
                magtag.peripherals.neopixels[i] = self.ledColors[i]
            magtag.peripherals.neopixels.show()

            if self.dirty and self.readyToDraw():
                aqis = []
                if self.aqiIn is not None:
                    aqis.append('In: {inside:.0f}'.format(inside=self.aqiIn))
                if self.aqiOut is not None:
                    aqis.append('Out: {outside:.0f}'.format(outside=self.aqiOut))
                magtag.set_text('AQI ' + (', '.join(aqis)), 2, False)
                if self.co2 is not None:
                    magtag.set_text('CO2: {co2:.0f} ppm'.format(co2=self.co2), 3, False)
                magtag.set_text(self.wind, 0, False)
                magtag.set_text('{time}                 {bat:.2f}V'.format(time=self.time, bat=self.volts), 1, False)
                magtag.refresh()
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
        duration = 30
        if t != 'on': return

        prevColor = self.ledColors[1]
        def blink3(): self.blink(1, (127, 0, 0))
        def resume3(): self.ledColors[1] = prevColor

        self.ledColors[1] = (127, 0, 0)
        schedule.every(0.5).seconds.until(timedelta(seconds=duration)).do(blink3)
        schedule.every(duration+1).seconds.until(timedelta(seconds=duration+5)).do(resume3)

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

    # 1: Time and Voltage
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

    # magtag.set_text('Wind: {wind}'.format(wind=self.wind), 4, False)

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
    print("Subscribing to a bunch of junk")
    state.mqtt_client.subscribe(AQI_TOPIC)
    state.mqtt_client.subscribe(TIME_TOPIC)
    state.mqtt_client.subscribe(PERIOD_TOPIC)
    state.mqtt_client.subscribe(NETSTATE_TOPIC)
    state.mqtt_client.subscribe(DISPLAY_TOPIC)
    state.mqtt_client.subscribe(DOORBELL_TOPIC)
    state.mqtt_client.subscribe(PW_STATE_TOPIC)
    state.mqtt_client.subscribe(ACTIVITY_TOPIC)
    state.mqtt_client.subscribe(WIND_TOPIC)
    state.mqtt_client.subscribe(WINDD_TOPIC)

def handleButtons():
    for l in ['a', 'b', 'c', 'd']:
        if getattr(magtag.peripherals, 'button_' + l + '_pressed'):
            state.mqtt_client.publish(BUTTON_TOPIC + l, 1, qos=1)
    state.mqtt_client.loop()

def main():
    w.feed()
    init()

    magtag.peripherals.neopixels.fill((0, 0, 0))
    schedule.run_all()

    while True:
        if magtag.peripherals.any_button_pressed:
            handleButtons()
        schedule.run_pending()
        state.draw()
        time.sleep(0.1) # schedule.idle_seconds())

        if state.volts < 3.5:
            print("doing a deep sleep")
            magtag.peripherals.neopixels.fill((0, 0, 0))
            magtag.peripherals.neopixel_disable = True
            w.deinit()
            magtag.exit_and_deep_sleep(900)

# main()

while True:
    try:
        main()
    except:
        print("oh no: exception")
        cylon((16,0,0))
    for i in range(60):
        w.feed()
        time.sleep(1)
