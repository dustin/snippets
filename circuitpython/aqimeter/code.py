from adafruit_magtag.magtag import MagTag
import adafruit_minimqtt.adafruit_minimqtt as MQTT
import socketpool
import ssl
import terminalio
import time
import wifi

try:
    from secrets import secrets
except ImportError:
    print("WiFi secrets are kept in secrets.py, please add them there!")
    raise

magtag = MagTag()

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

def message(client, topic, message):
    magtag.peripherals.neopixels.fill((0, 0, 0))
    magtag.peripherals.neopixel_disable = True

    magtag.set_text('{aqi:.0f}'.format(aqi=float(message)), 0, False)
    magtag.set_text('Battery: {bat:.2f}V'.format(bat=magtag.peripherals.battery), 1)

    time.sleep(2)
    magtag.exit_and_deep_sleep(900)

pool = socketpool.SocketPool(wifi.radio)

mqtt_client = MQTT.MQTT(
    broker=secrets["broker"],
    port=secrets["port"],
    username=secrets["mqtt_username"],
    password=secrets["mqtt_pw"],
    socket_pool=pool,
)

mqtt_client.on_message = message
mqtt_client.connect()
mqtt_client.subscribe("oro/purpleair/aqi")
mqtt_client.loop()


while not done:
    try:
        mqtt_client.loop()
    except (ValueError, RuntimeError) as e:
        print("Failed to get data, retrying\n", e)
        wifi.reset()
        mqtt_client.reconnect()
        continue
    time.sleep(1)
