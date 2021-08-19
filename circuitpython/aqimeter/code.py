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

magtag = MagTag(url='http://192.168.2.170/json',
                json_path=[["pm2.5_aqi"], ["pm2.5_aqi_b"], ["current_temp_f"]],
                # json_transform=lambda d: d['aqi'] = (val[0] + val[1]) / 2.0
                )

magtag.peripherals.neopixel_disable = False
magtag.peripherals.neopixels.fill((8, 0, 0))
wifi.radio.connect(secrets["ssid"], secrets["password"])
magtag.peripherals.neopixels.fill((6, 3, 16))

val = magtag.fetch(auto_refresh=False)
magtag.peripherals.neopixels.fill((0, 0, 0))
magtag.peripherals.neopixel_disable = True

magtag.add_text(
    text_font="/fonts/Helvetica-Bold-100.bdf",
    # text_font=terminalio.FONT,
    # text_position=(70, 40),
    text_position=(
        (magtag.graphics.display.width // 2) - 1,
        (magtag.graphics.display.height // 2) - 22,
    ),
    text_anchor_point=(0.5, 0.5),
)

magtag.add_text(
    text_font="/fonts/Arial-Bold-12.pcf",
    # text_font=terminalio.FONT,
    text_position=(10, magtag.graphics.display.height - 14),
)

magtag.set_text('{aqi:.0f}'.format(aqi=(val[0] + val[1]) / 2.0), 0, False)
magtag.set_text('Battery: {bat:.2f}V'.format(bat=magtag.peripherals.battery), 1)

time.sleep(2)

magtag.exit_and_deep_sleep(900)
