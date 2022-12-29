from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import time
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode
import time
import circuitpython_schedule as schedule

import busio
import board

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
except:
    scd = None

try:
    from secrets import secrets
except ImportError:
    print("WiFi secrets are kept in secrets.py, please add them there!")
    raise

AQI_TOPIC="home/purpleair/aqi"
TIME_TOPIC="home/local/time"
NETSTATE_TOPIC="home/ping/8.8.8.8/label"
PERIOD_TOPIC="home/magtag/period"
VOLT_TOPIC="home/magtag/{mqtt_username}/voltage".format(**secrets)
BAT_TOPIC="home/magtag/{mqtt_username}/battery".format(**secrets)
PM25_TOPIC="home/magtag/{mqtt_username}/pm2.5".format(**secrets)
CO2_TOPIC="home/magtag/{mqtt_username}/co2".format(**secrets)
TEMP_TOPIC="home/magtag/{mqtt_username}/temperature".format(**secrets)
HUMIDITY_TOPIC="home/magtag/{mqtt_username}/humidity".format(**secrets)
DUR_TOPIC="home/magtag/{mqtt_username}/duration".format(**secrets)
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

schedule.every(1).seconds.do(w.feed)

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
        self.purpleAQI = None
        self.netState = None
        self.volts = None

        pool = socketpool.SocketPool(wifi.radio)

        self.mqtt_client = MQTT.MQTT(
            broker=secrets["broker"],
            port=secrets["port"],
            username=secrets["mqtt_username"],
            password=secrets["mqtt_pw"],
            socket_pool=pool,
        )

    def gotTime(self, client, topic, t):
        self.time = t

    def gotAQI(self, client, topic, a):
        self.purpleAQI = float(a)
        self.dirty = True

    def updatePM25(self):
        if pm25:
            aqdata = pm25.read()
            # See also "pm10 standard", "pm100 standard", "pm10 env", "pm25 env", "pm100 env"
            self.mqtt_client.publish(PM25_TOPIC, aqdata["pm25 standard"], retain=True)
            magtag.set_text('Inside: {aqi}'.format(aqi=aqdata["pm25 standard"]), 2, False)
            self.dirty = True

    def updateCO2(self):
        if scd and scd.data_available and scd.CO2 > 0:
            self.mqtt_client.publish(CO2_TOPIC, scd.CO2, retain=True)
            self.mqtt_client.publish(TEMP_TOPIC, scd.temperature, retain=True)
            self.mqtt_client.publish(HUMIDITY_TOPIC, scd.relative_humidity, retain=True)
            magtag.set_text('CO2: {co2:.0f} ppm'.format(co2=scd.CO2), 3, False)
            self.dirty = True

    def updateBattery(self):
        self.volts = magtag.peripherals.battery
        self.mqtt_client.publish(VOLT_TOPIC, self.volts, retain=True)
        self.mqtt_client.publish(BAT_TOPIC, min(100, self.volts*100 / 4.2), retain=True)

    def gotNetState(self, client, topic, t):
        self.netState = t
        colors = {'ok': (0, 8, 0),
                  'slow': (255, 191, 0),
                  'loss': (255, 0, 0)}
        magtag.peripherals.neopixels.fill(colors[t])

    def draw(self):
        if self.dirty and self.time is not None and self.purpleAQI is not None and self.volts is not None:
            magtag.set_text('{aqi:.0f}'.format(aqi=self.purpleAQI), 0, False)
            magtag.set_text('{time}                 {bat:.2f}V'.format(time=self.time, bat=self.volts), 1, False)
            magtag.refresh()
            self.dirty = False

state = State()

def init():
    schedule.every(60).seconds.do(state.updatePM25)
    schedule.every(60).seconds.do(state.updateCO2)
    schedule.every(60).seconds.do(state.updateBattery)
    # The outside AQI
    magtag.add_text(
        text_font="/fonts/Helvetica-Bold-100.bdf",
        text_position=(
            (magtag.graphics.display.width // 2) - 1,
            (magtag.graphics.display.height // 2) - 10,
        ),
        text_anchor_point=(0.5, 0.5),
    )

    # Time and Voltage
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, magtag.graphics.display.height - 14),
    )

    # Indoor AQI and stuff.
    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(6, 2),
        text_anchor_point=(0, 0)
    )

    # CO2
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
    state.mqtt_client.connect()
    state.mqtt_client.subscribe(AQI_TOPIC)
    state.mqtt_client.subscribe(TIME_TOPIC)
    state.mqtt_client.subscribe(PERIOD_TOPIC)
    schedule.every(60).seconds.do(state.mqtt_client.loop)


def main():
    w.feed()
    init()
    schedule.run_all()
    magtag.peripherals.neopixels.fill((0, 0, 0))
    state.mqtt_client.subscribe(NETSTATE_TOPIC)

    while True:
        schedule.run_pending()
        state.draw()
        time.sleep(1) # schedule.idle_seconds())

        if state.volts < 3.5:
            print("doing a deep sleep")
            magtag.peripherals.neopixels.fill((0, 0, 0))
            magtag.peripherals.neopixel_disable = True
            w.deinit()
            magtag.exit_and_deep_sleep(900)

main()

while True:
    try:
        main()
    except:
        print("oh no: exception")
        cylon((16,0,0))
    for i in range(60):
        w.feed()
        time.sleep(1)
