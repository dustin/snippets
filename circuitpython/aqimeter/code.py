from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import time
import wifi
from microcontroller import watchdog as w
from watchdog import WatchDogMode
import time

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


def main():
    w.feed()
    startTime = time.monotonic()

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

    timeAndAQI = ["", -1, 0]

    def isReady():
        return timeAndAQI[0] != "" and timeAndAQI[1] != -1 and timeAndAQI[2] != 0

    def gotAQI(client, topic, message):
        timeAndAQI[1] = float(message)

    def gotTime(client, topic, message):
        timeAndAQI[0] = message

    def gotPeriod(client, topic, message):
        global sleepTime
        sleepTime = int(message)
        timeAndAQI[2] = sleepTime

    pool = socketpool.SocketPool(wifi.radio)

    mqtt_client = MQTT.MQTT(
        broker=secrets["broker"],
        port=secrets["port"],
        username=secrets["mqtt_username"],
        password=secrets["mqtt_pw"],
        socket_pool=pool,
    )

    volts = magtag.peripherals.battery

    mqtt_client.add_topic_callback(AQI_TOPIC, gotAQI)
    mqtt_client.add_topic_callback(TIME_TOPIC, gotTime)
    mqtt_client.add_topic_callback(PERIOD_TOPIC, gotPeriod)
    mqtt_client.connect()
    mqtt_client.subscribe(AQI_TOPIC)
    mqtt_client.subscribe(TIME_TOPIC)
    mqtt_client.subscribe(PERIOD_TOPIC)
    mqtt_client.publish(VOLT_TOPIC, volts, retain=True)
    mqtt_client.publish(BAT_TOPIC, min(100, volts*100 / 4.2), retain=True)

    w.feed()

    if pm25:
        aqdata = pm25.read()
        # See also "pm10 standard", "pm100 standard", "pm10 env", "pm25 env", "pm100 env"
        mqtt_client.publish(PM25_TOPIC, aqdata["pm25 standard"], retain=True)
        magtag.set_text('Inside: {aqi}'.format(aqi=aqdata["pm25 standard"]), 2, False)

    w.feed()

    if scd and scd.data_available and scd.CO2 > 0:
        mqtt_client.publish(CO2_TOPIC, scd.CO2, retain=True)
        mqtt_client.publish(TEMP_TOPIC, scd.temperature, retain=True)
        mqtt_client.publish(HUMIDITY_TOPIC, scd.relative_humidity, retain=True)
        magtag.set_text('CO2: {co2:.0f} ppm'.format(co2=scd.CO2), 3, False)


    w.feed()
    for i in range (0, 30):
        mqtt_client.loop()

        if isReady():
            break
        time.sleep(1)

    w.feed()
    finished = time.monotonic()
    print("time spent: ", finished)
    mqtt_client.publish(DUR_TOPIC, finished)

    if not isReady():
        cylon((6,3,16))
        return

    magtag.peripherals.neopixels.fill((0, 0, 0))
    magtag.peripherals.neopixel_disable = True

    magtag.set_text('{aqi:.0f}'.format(aqi=timeAndAQI[1]), 0, False)
    magtag.set_text('{time}                 {bat:.2f}V'.format(time=timeAndAQI[0], bat=volts), 1, False)

    magtag.refresh()

    w.feed()
    time.sleep(2)
    if volts > 4.1:
        global sleepTime
        sleepTime = sleepTime / 10


main()

try:
    main()
except:
    print("oh no: exception")
    cylon((16,0,0))

w.deinit()
print("Sleeping for", sleepTime)
magtag.exit_and_deep_sleep(sleepTime)
