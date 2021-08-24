from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import time
import wifi

try:
    from secrets import secrets
except ImportError:
    print("WiFi secrets are kept in secrets.py, please add them there!")
    raise

AQI_TOPIC="oro/purpleair/aqi"
TIME_TOPIC="oro/local/time"
VOLT_TOPIC="oro/magtag/{mqtt_username}/voltage".format(**secrets)
BAT_TOPIC="oro/magtag/{mqtt_username}/battery".format(**secrets)
SLEEP_TIME=900
MIN_LIGHT=500

magtag = MagTag()

def cylon(color):
    for i in [3, 2, 1, 0, 1, 2, 3]:
        magtag.peripherals.neopixels[i] = color
        time.sleep(0.1)
        magtag.peripherals.neopixels[i] = (0, 0, 0)
        magtag.peripherals.neopixels.show()


# Before we do anything of interest, check to see if the light's on.
# If it's dark, we shouldn't do anything.
magtag.peripherals.neopixels.fill((0, 0, 0))
magtag.peripherals.neopixel_disable = False
if magtag.peripherals.light < MIN_LIGHT:
    print("Light is {0}, guess I'll sleep now".format(magtag.peripherals.light))
    cylon((4,0,0))
    magtag.exit_and_deep_sleep(60)


def main():
    magtag.add_text(
        text_font="/fonts/Helvetica-Bold-100.bdf",
        text_position=(
            (magtag.graphics.display.width // 2) - 1,
            (magtag.graphics.display.height // 2) - 22,
        ),
        text_anchor_point=(0.5, 0.5),
    )

    magtag.add_text(
        text_font="/fonts/Arial-Bold-12.pcf",
        text_position=(10, magtag.graphics.display.height - 14),
    )

    magtag.peripherals.neopixel_disable = False
    magtag.peripherals.neopixels.fill((8, 0, 0))
    wifi.radio.connect(secrets["ssid"], secrets["password"])
    magtag.peripherals.neopixels.fill((6, 3, 16))

    timeAndAQI = ["", -1]

    def isReady():
        return timeAndAQI[0] != "" and timeAndAQI[1] != -1

    def gotAQI(client, topic, message):
        timeAndAQI[1] = float(message)

    def gotTime(client, topic, message):
        timeAndAQI[0] = message

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
    mqtt_client.connect()
    mqtt_client.publish(VOLT_TOPIC, volts, retain=True)
    mqtt_client.publish(BAT_TOPIC, min(100, volts*100 / 4.2), retain=True)
    mqtt_client.subscribe(AQI_TOPIC)
    mqtt_client.subscribe(TIME_TOPIC)

    for i in range (0, 30):
        mqtt_client.loop()

        if isReady():
            break
        time.sleep(1)

    if not isReady():
        cylon((6,3,16))
        return

    magtag.peripherals.neopixels.fill((0, 0, 0))
    magtag.peripherals.neopixel_disable = True

    magtag.set_text('{aqi:.0f}'.format(aqi=timeAndAQI[1]), 0, False)
    magtag.set_text('{time}                {bat:.2f}V'.format(time=timeAndAQI[0], bat=volts), 1, False)

    magtag.refresh()

    time.sleep(2)

try:
    main()
    magtag.exit_and_deep_sleep(SLEEP_TIME)
except:
    cylon((64,0,0))
    magtag.exit_and_deep_sleep(60)

