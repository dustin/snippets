from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import time
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode
import time
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

sleepTime=900

w.timeout=10.0
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


class State:
    def __init__(self):
        self.dirty = False
        self.time = None
        self.windVals = {}
        self.wind = 'unkn'
        self.stations = ['kihei']
        self.current = 'kihei'
        self.canRedraw = True
        self.display = True
        self.ledColors = [(0, 0, 0)] * 4
        self.volts = None

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
        return self.canRedraw and self.time is not None and self.volts is not None

    def draw(self):
        if self.display:
            for i in range(4):
                magtag.peripherals.neopixels[i] = self.ledColors[i]
            magtag.peripherals.neopixels.show()

            if self.dirty and self.readyToDraw():
                wv = self.windVals[self.current]
                locName = 'Kihei'
                if self.current in self.windVals:
                    locName = wv['label']
                print("Drawing ", locName)
                magtag.set_text(locName, 2, False)
                magtag.set_text(self.wind, 0, False)
                magtag.set_text('{dir_card} {dir_deg}°'.format(**wv), 3, False)
                magtag.set_text('{time}                 {bat:.2f}V'.format(time=self.time, bat=self.volts), 1, False)
                magtag.refresh()
                # self.mqtt_client.publish(INFO_TOPIC, 'drawing')
                self.dirty = False
                self.canRedraw = False

    def forceDraw(self):
        self.dirty = True
        self.canRedraw = True
        self.updateWindText()
        self.draw()

    def updateWindText(self):
        w = self.windVals[self.current]
        d = '{avg}g{gust}'.format(avg=round(w['avg']), gust=round(w['gust']))
        if d != self.wind:
            self.wind = d
            self.dirty = True

    def gotWind(self, client, topic, t):
        w = json.loads(t)
        self.windVals[w['shortLabel']] = w
        if w['shortLabel'] not in self.stations:
            self.stations.append(w['shortLabel'])
            self.stations = sorted(self.stations)
            print("Stations now:", self.stations)

        if w['shortLabel'] == self.current:
            self.updateWindText()

    def allowRedraw(self):
        self.canRedraw = True

    def gotTime(self, client, topic, t):
        self.time = t

    def gotAQI(self, client, topic, a):
        print("got AQI", a)
        try:
            self.aqiOut = float(a)
            self.dirty = True
        except (ValueError, TypeError):
            print("Invalid AQI value:", a)

    def updateBattery(self):
        self.volts = magtag.peripherals.battery
        self.mqtt_client.publish(VOLT_TOPIC, self.volts, retain=True)
        self.mqtt_client.publish(BAT_TOPIC, min(100, self.volts*100 / 4.2), retain=True)
        self.mqtt_client.loop()

    def nextStation(self):
        c = self.stations.index(self.current)
        n = (c + 1) % len(self.stations)
        self.current = self.stations[n]
        print("Changed to station ", self.current)
        self.forceDraw()

state = State()
schedule.every(1).seconds.do(w.feed)
schedule.every(1).seconds.do(state.mqtt_loop)
schedule.every(60).seconds.do(state.updateBattery)
schedule.every(60).seconds.do(state.allowRedraw)

def main():
    w.feed()
    startTime = time.monotonic()


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

    timeAndAQI = ["", -1, 0]

    pool = socketpool.SocketPool(wifi.radio)

    volts = magtag.peripherals.battery

    state.mqtt_client.add_topic_callback(TIME_TOPIC, state.gotTime)
    state.mqtt_client.add_topic_callback(WIND_TOPIC, state.gotWind)
    state.mqtt_client.connect()
    state.mqtt_client.subscribe(TIME_TOPIC)
    state.mqtt_client.subscribe(WIND_TOPIC)

    w.feed()

    magtag.peripherals.neopixels.fill((0, 0, 0))
    schedule.run_all()

    while True:
        if magtag.peripherals.button_c_pressed:
            state.nextStation()

        schedule.run_pending()
        state.draw()
        time.sleep(0.1)

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
