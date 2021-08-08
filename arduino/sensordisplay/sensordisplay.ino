
#include <CircularBuffer.h>

#include <ArduinoOTA.h>
#include <WiFi.h>
#include <WiFiClientSecure.h>
#include <MQTT.h>
#include <time.h>

#include <SPI.h>
#include <Adafruit_GFX.h>
#include <Adafruit_ILI9341.h>
#include <Adafruit_STMPE610.h>

const char* ssid = "Spy Wireless IX";
const char* password = "monstercable";
const char* myname = "sensordisplay";

const char* mqttServer = "mqtt";
const char* mqttUser = "display";
const char* mqttAuth = "";

const char* aqiTopic = "oro/purpleair/aqi";


const char* inTopic = "na";

#define FONT_WIDTH 5
#define FONT_HEIGHT 8

#ifdef ESP32
# define STMPE_CS 32
# define TFT_CS   15
# define TFT_DC   33
# define SD_CS    14
#endif
#ifdef ARDUINO_STM32_FEATHER
# define TFT_DC   PB4
# define TFT_CS   PA15
# define STMPE_CS PC7
# define SD_CS    PC5
#endif

#define BACKLIGHT 21
#define BATTERY_PIN A13

WiFiClient wiCli;
MQTTClient client;

Adafruit_ILI9341 tft = Adafruit_ILI9341(TFT_CS, TFT_DC);
Adafruit_STMPE610 touch = Adafruit_STMPE610(STMPE_CS);

// This is basically used to determine how long the display is lit.
static time_t latestMod(0);

#define BASE_COLOR ILI9341_GREEN

// How long (in seconds) errors can stay on the screen.
#define OLDEST_ERROR   (60*30)
#define OLDEST_READING (60*20)

#define READING_ROW 2
#define HUMIDITY_COLUMN 12

void age_str(char *buf, size_t buflen, time_t now, time_t ts);

class TimedError {
public:
    TimedError(String m) : msg(m), ts(time(NULL)) {}
    TimedError() : TimedError("") {}

    String msg;
    time_t ts;
};

class Widget {
public:
    Widget(int xpos, int ypos) : x(xpos), y(ypos), modtime(0) {}

    virtual void render(time_t now) = 0;

    double staleness(time_t now) {
        return difftime(now, modtime);
    }

    int x, y;
    time_t modtime;
};

class TextWidget : public Widget {
public:
    TextWidget(int x, int y, uint8_t s, uint16_t c, String m) : Widget(x, y), size(s), color(c), msg(m) {}

    void render(time_t now) {
        tft.setCursor(x, y);
        tft.setTextSize(size);
        tft.setTextColor(color, ILI9341_BLACK);
        tft.print(msg);
    }

private:
    uint8_t size;
    uint16_t color;
    String msg;
};

class ErrorWidget : public Widget {
public:

    ErrorWidget(int x, int y, int m) : Widget(x, y), maxHeight(m), changed(false) {}

    void render(time_t now) {
        if (!changed && errors.size() == 0) {
            return;
        }

        renderAge(now);

        pruneErrors(now);

        if (staleness(now) < 1 || !changed) {
            return;
        }

        tft.fillRect(x, y, 320-x, 240-FONT_HEIGHT*2-y, ILI9341_BLACK);

        tft.setCursor(0, y+FONT_HEIGHT+2);
        tft.setTextSize(2);

        int height(0);
        for (unsigned int i = errors.size(); i > 0; i--) {
            auto e = errors[i-1].msg;
            int rows = 1 +  e.length() / 27; // room for 26 at font size 3; fair dice roll.
            height += (3 * FONT_HEIGHT * rows);

            if (height > maxHeight) {
                break;
            }

            tft.println(errors[i-1].msg);
        }
        modtime = now;
        changed = false;
    }

    void renderAge(time_t now) {
        tft.setCursor(0, y);

        tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
        tft.setTextSize(1);
        char buf[10];
        age_str(buf, sizeof(buf), now, errors.last().ts);
        tft.print(buf);
    }

    void pruneErrors(time_t now) {
        int pruned(0);
        // Kill off old errors.
        while (errors.size() > 0 && difftime(now, errors.first().ts) > OLDEST_ERROR) {
            pruned++;
            errors.shift();
        }
        if (pruned > 0) {
            changed = true;
        }
    }

    void addError(String s) {
        TimedError e(s);
        errors.push(e);
        changed = true;
    }

    CircularBuffer<TimedError, 5> errors;
    int maxHeight;
    bool changed;
};

ErrorWidget errors(0, FONT_HEIGHT*3*5, FONT_HEIGHT*3*5);

void age_str(char *buf, size_t buflen, time_t now, time_t ts) {
    double age = difftime(now, ts);
    int mins = age / 60;
    int secs = (int)age % 60;
    snprintf(buf, buflen, "-%d:%02d ", mins, secs);
}

/*
 * Format an unsigned long (32 bits) into a string in the format
 * "23,854,972".
 *
 * The provided buffer must be at least 14 bytes long. The number will
 * be right-adjusted in the buffer. Returns a pointer to the first
 * digit.
 */
static char *vtoa(int val, char *s)
{
    char *p = s + 13;
    *p = '\0';
    do {
        if ((p - s) % 4 == 2)
            *--p = ',';
        *--p = '0' + val % 10;
        val /= 10;
    } while (val);
    return p;
}

template <class T>
class SensorValue : public Widget {
public:
    SensorValue(int xpos, int ypos, T l, T h, char c) : Widget(xpos, ypos),
                                                        v(0),
                                                        low(l), high(h),
                                                        sym(c), ts(0) {}

    void render(time_t now) {
        prune(now);

        if (staleness(now) < 1) {
            return;
        }

        tft.setCursor(x, y);
        tft.setTextSize(5);
        if (!hasValue()) {
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
            tft.fillRect(x, y, FONT_WIDTH*3*8, FONT_HEIGHT*3 + FONT_HEIGHT*2, ILI9341_BLACK);
        } else {
            if (v < low) {
                tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
            } else if (v > high) {
                tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
            } else {
                tft.setTextColor(ILI9341_YELLOW, ILI9341_BLACK);
            }

            char buf[16];
            tft.print(vtoa(v, buf));
            tft.print(sym);

            // Show a timestamp on the next line.
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
            tft.setCursor(x, y+FONT_HEIGHT*5 + 2);
            struct tm tmts;
            localtime_r(&ts, &tmts);

            strftime(buf, sizeof(buf)-1, "%H:%M  ", &tmts);
            tft.setTextSize(2);
            tft.print(buf);

            tft.setTextSize(1);
            tft.setCursor(x + FONT_WIDTH*2*6 + 4, y+FONT_HEIGHT*5 + 8);
            age_str(buf, sizeof(buf), now, ts);
            tft.print(buf);
        }
        modtime = now;
    }

    void update(T f, time_t when) {
        v = f;
        ts = when;
        modtime = 0;
    }

    void prune(time_t now) {
        double age = difftime(now, ts);
        if (hasValue() && age > 86400) {
            v = 0;
        }
    }

    bool hasValue() {
        return !(isnan(v) || v == 0);
    }

    T  v;
    T low, high;
    char sym;
    time_t ts;
};

class TimeWidget : public Widget {
public:
    TimeWidget(int x, int y) : Widget(x, y) {}

    void render(time_t t) {
        if (staleness(t) < 1) {
            return;
        }

        tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
        tft.setCursor(x, y);
        struct tm tmts;
        localtime_r(&t, &tmts);
        char buf[24];
        strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%S", &tmts);
        tft.setTextSize(2);
        tft.print(buf);
        modtime = t;
    }
};

class WiFiWidget : public Widget {
public:
    WiFiWidget(int x, int y) : Widget(x, y), status(-999) {}

    void render(time_t t) {
        if (staleness(t) < 1) {
            return;
        }
        modtime = t;

        tft.setCursor(x, y);
        tft.setTextSize(2);
        tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);

        int st = WiFi.status();

        if (status != st) {
            tft.fillRect(0, y, 320, FONT_HEIGHT*3, ILI9341_BLACK);
        }

        status = st;

        if (st != WL_CONNECTED) {
            showStatus();
            return;
        }

        char buf[(320 / (FONT_WIDTH*2)) + 1];
        String ssid = WiFi.SSID();
        #define MAX_SSID_LEN 19
        if (ssid.length() > MAX_SSID_LEN) {
            ssid = ssid.substring(ssid.length() - MAX_SSID_LEN);
        }
        snprintf(buf, sizeof(buf), "%-19s %ddBm", ssid.c_str(), WiFi.RSSI());
        tft.print(buf);
    }

    void showStatus() {
        tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
        tft.setCursor(x + FONT_WIDTH*2 + 13, y+4);
        tft.print("wifi: ");

        switch (status) {
        case WL_CONNECTED: tft.print("connected"); break;
        case WL_NO_SHIELD: tft.print("no shield"); break;
        case WL_IDLE_STATUS: tft.print("idle"); break;
        case WL_NO_SSID_AVAIL: tft.print("no available SSID"); break;
        case WL_SCAN_COMPLETED: tft.print("scan complete"); break;
        case WL_CONNECT_FAILED: tft.print("connect failed"); break;
        case WL_CONNECTION_LOST: tft.print("conn lost"); break;
        case WL_DISCONNECTED: tft.print("disconnected"); break;
        }
    }

    int status;
};

class BatteryWidget : public Widget {
public:
    BatteryWidget(int x, int y) : Widget(x, y) {}

    void render(time_t t) {
        if (staleness(t) < 5) {
            return;
        }
        modtime = t;

        auto r = analogRead(BATTERY_PIN);
        float halfv = 3.30f * float(r) / 4096.0f;
        // auto halfv = r * (3.0 / 4096.0);
        _reading = halfv * 2;

        tft.setCursor(x, y);
        if (_reading > 3.8) {
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
        } else if (_reading > 3.6) {
            tft.setTextColor(ILI9341_YELLOW, ILI9341_BLACK);
        } else {
            tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
        }

        char buf[8];
        snprintf(buf, sizeof(buf), "%.2fV", _reading);
        tft.print(buf);
    }

    float reading() { return _reading; }
private:
    float _reading;
};

TextWidget aqiLabelWidget(0, FONT_HEIGHT*3*READING_ROW, 3, ILI9341_OLIVE, "AQI ");
SensorValue<int> aqiWidget(FONT_WIDTH*3*5, FONT_HEIGHT*3*READING_ROW, 100, 180, ' ');

TimeWidget timeWidget(320-(FONT_WIDTH*2*23), 240-FONT_HEIGHT*2);

WiFiWidget wifiWidget(0, 0);

BatteryWidget batteryWidget(0, 240-FONT_HEIGHT*2);

Widget* widgets[] = {&aqiLabelWidget, &aqiWidget, &timeWidget, &errors, &wifiWidget, &batteryWidget, nullptr};

void setup() {
    Serial.begin(115200);
    setupDisplay();
    setupWifi();
    setupOTA();
    setupTime();
    setupMQTT();

    pinMode(BATTERY_PIN, INPUT);
}

void setupOTA() {
    ArduinoOTA.setHostname("sensordisplay");

    ArduinoOTA
        .onStart([]() {
                     String type;
                     if (ArduinoOTA.getCommand() == U_FLASH)
                         type = "sketch";
                     else // U_SPIFFS
                         type = "filesystem";
                     // NOTE: if updating SPIFFS this would be the place to unmount SPIFFS using SPIFFS.end()
                     tft.fillScreen(ILI9341_BLACK);
                     statusMessage("Starting update");
                     // client.publish(errFeed, "display OTA upgrade beginning");
                     tft.setTextSize(3);
                     tft.setTextColor(ILI9341_BLUE, ILI9341_BLACK);
                     digitalWrite(BACKLIGHT, HIGH);
                 })
        .onEnd([]() {
                   statusMessage("OTA Complete");
               })
        .onProgress([](unsigned int progress, unsigned int total) {
                        char buf[80];
                        float complete = (float)progress / (float)total;
                        snprintf(buf, sizeof(buf), "Progress: %.0f%%    ", 100.0f*complete);
                        tft.setCursor(0, FONT_HEIGHT*3);
                        tft.fillRect(0, 0, (int)(320.0f * complete), 5, ILI9341_BLUE);
                        tft.print(buf);
                    })
        .onError([](ota_error_t error) {
                     switch (error) {
                     case OTA_AUTH_ERROR: statusMessage("Error: Auth Failed"); break;
                     case OTA_BEGIN_ERROR:  statusMessage("Error: Begin Failed"); break;
                     case OTA_CONNECT_ERROR: statusMessage("Error: Connect Failed"); break;
                     case OTA_RECEIVE_ERROR: statusMessage("Error: Receive Failed"); break;
                     case OTA_END_ERROR: statusMessage("Error: End Failed"); break;
                     default: statusMessage(String("Error #") + error);
                     }
                 });
    ArduinoOTA.begin();
}

void statusMessage(String msg) {
    uint16_t topy = FONT_HEIGHT*3; // one row down at 3x
    tft.setTextSize(2);
    tft.fillRect(0, topy, 320, FONT_HEIGHT*3, ILI9341_BLACK);
    tft.setCursor(0, topy);
    tft.print(msg);
 }

void setupTime() {
    configTime(0 /* tz */, 0 /* dst */, "pool.ntp.org");
    Serial.println(F("Waiting for time sync..."));
    statusMessage("syncing time");
    while (time(nullptr) < 1535920965) {
        delay(10);
    }
    setenv("TZ", "PST8PDT7,M3.2.0/02:00:00,M11.1.0/02:00:00", 1);
    tzset();

    latestMod = time(NULL);
    Serial.print(F("Initial time: "));
    Serial.println(latestMod);
}


void setupDisplay() {
    pinMode(BACKLIGHT, OUTPUT);
    digitalWrite(BACKLIGHT, HIGH);

    tft.begin();
    tft.setRotation(1);
    tft.setTextSize(3);
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);

    // read diagnostics (optional but can help debug problems)
    uint8_t x = tft.readcommand8(ILI9341_RDMODE);
    Serial.print(F("Display Power Mode: 0x")); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDMADCTL);
    Serial.print(F("MADCTL Mode: 0x")); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDPIXFMT);
    Serial.print(F("Pixel Format: 0x")); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDIMGFMT);
    Serial.print(F("Image Format: 0x")); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDSELFDIAG);
    Serial.print(F("Self Diagnostic: 0x")); Serial.println(x, HEX);

    tft.fillScreen(ILI9341_BLACK);

    if (! touch.begin()) {
        Serial.println(F("Touch screen not found"));
    }
}

String wifiEventString(WiFiEvent_t e) {
    switch(e) {
    case SYSTEM_EVENT_WIFI_READY: return "WiFi ready";
    case SYSTEM_EVENT_SCAN_DONE: return "finish scanning AP";
    case SYSTEM_EVENT_STA_START: return "station start";
    case SYSTEM_EVENT_STA_STOP: return "station stop";
    case SYSTEM_EVENT_STA_CONNECTED: return "station connected to AP";
    case SYSTEM_EVENT_STA_DISCONNECTED: return "station disconnected from AP";
    case SYSTEM_EVENT_STA_AUTHMODE_CHANGE: return "the auth mode of AP connected by ESP32 station changed";
    case SYSTEM_EVENT_STA_GOT_IP: return "station got IP from connected AP";
    case SYSTEM_EVENT_STA_LOST_IP: return "station lost IP and the IP is reset to 0";
    case SYSTEM_EVENT_STA_WPS_ER_SUCCESS: return "station wps succeeds in enrollee mode";
    case SYSTEM_EVENT_STA_WPS_ER_FAILED: return "station wps fails in enrollee mode";
    case SYSTEM_EVENT_STA_WPS_ER_TIMEOUT: return "station wps timeout in enrollee mode";
    case SYSTEM_EVENT_STA_WPS_ER_PIN: return "station wps pin code in enrollee mode";
    case SYSTEM_EVENT_AP_START: return "soft-AP start";
    case SYSTEM_EVENT_AP_STOP: return "soft-AP stop";
    case SYSTEM_EVENT_AP_STACONNECTED: return "a station connected to ESP32 soft-AP";
    case SYSTEM_EVENT_AP_STADISCONNECTED: return "a station disconnected from ESP32 soft-AP";
    case SYSTEM_EVENT_AP_PROBEREQRECVED: return "Receive probe request packet in soft-AP interface";
    case SYSTEM_EVENT_GOT_IP6: return "station or ap or ethernet interface v6IP addr is preferred";
    case SYSTEM_EVENT_ETH_START: return "ethernet start";
    case SYSTEM_EVENT_ETH_STOP: return "ethernet stop";
    case SYSTEM_EVENT_ETH_CONNECTED: return "ethernet phy link up";
    case SYSTEM_EVENT_ETH_DISCONNECTED: return "ethernet phy link down";
    case SYSTEM_EVENT_ETH_GOT_IP: return "ethernet got IP from connected AP";
    case SYSTEM_EVENT_AP_STAIPASSIGNED: return "IP assigned";
    case SYSTEM_EVENT_MAX: return "EVENT_MAX is not a real value.";
    }
    return "Unknown event";
}

void setupWifi() {
    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(LED_BUILTIN, LOW); // low is on, night is day

    tft.setCursor(0, 0);
    tft.println("connecting...");

    WiFi.mode(WIFI_STA);
    WiFi.onEvent([](WiFiEvent_t e) { statusMessage(wifiEventString(e)); });

    WiFi.begin(ssid, password);
    while (WiFi.status() != WL_CONNECTED) {
        digitalWrite(LED_BUILTIN, LOW);
        delay(500);
        digitalWrite(LED_BUILTIN, HIGH);
        delay(500);
    }

    Serial.println(F("IP address: "));
    Serial.println(WiFi.localIP());
    tft.fillScreen(ILI9341_BLACK);
    tft.setCursor(0, 0);
    tft.println(WiFi.localIP());

    digitalWrite(LED_BUILTIN, HIGH); // light's off when we're connected
}

void setupMQTT() {
    client.begin(mqttServer, 1883, wiCli);
    client.onMessage(messageReceived);
}

void appendBytes(String &s, const byte* payload, unsigned int length) {
    s.reserve(length);
    for (int i = 0; i < length; i++) {
        s.concat((char)payload[i]);
    }
}

void messageReceived(String &topic, String &payload) {
    Serial.print(F("Message arrived ["));
    Serial.print(topic);
    Serial.print(F("] "));
    Serial.println(payload);

    time_t now(time(NULL));
    latestMod = now;
    if (topic == aqiTopic) {
        aqiWidget.update(payload.toInt(), now);
    }
}

void showConnectionState() {
    uint16_t topy = FONT_HEIGHT*3; // one row down at 3x

    tft.fillRect(0, topy, 320, FONT_HEIGHT*3, ILI9341_BLACK);
    if (!client.connected()) {
        tft.setCursor(0, topy);
        tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
        tft.print("disco'd: ");
        tft.setTextSize(2);
        tft.print("reason: ");

        switch (client.lastError()) {
        case LWMQTT_SUCCESS: tft.print("success?"); break;
        case LWMQTT_BUFFER_TOO_SHORT: tft.print("buffer too short"); break;
        case LWMQTT_VARNUM_OVERFLOW: tft.print("varnum overflow"); break;
        case LWMQTT_NETWORK_FAILED_CONNECT: tft.print("failed connect"); break;
        case LWMQTT_NETWORK_TIMEOUT: tft.print("timeout"); break;
        case LWMQTT_NETWORK_FAILED_READ: tft.print("failed read"); break;
        case LWMQTT_NETWORK_FAILED_WRITE: tft.print("failed write"); break;
        case LWMQTT_REMAINING_LENGTH_OVERFLOW: tft.print("remaining length overflow"); break;
        case LWMQTT_REMAINING_LENGTH_MISMATCH: tft.print("remaining length mismatch"); break;
        case LWMQTT_MISSING_OR_WRONG_PACKET: tft.print("missing or wrong packet"); break;
        case LWMQTT_CONNECTION_DENIED: tft.print("connection denied"); break;
        case LWMQTT_FAILED_SUBSCRIPTION: tft.print("failed subscription"); break;
        case LWMQTT_SUBACK_ARRAY_OVERFLOW: tft.print("suback array overflow"); break;
        case LWMQTT_PONG_TIMEOUT: tft.print("pong timeout"); break;
        default:
            tft.print("unknown reason: ");
            tft.print(client.lastError());
        }
    }
    tft.setTextSize(3);
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
}

void checkTouch() {
    if (touch.touched()) {
        uint16_t x, y;
        uint8_t z;
        while (! touch.bufferEmpty()) {
            touch.readData(&x, &y, &z);
        }
        touch.writeRegister8(STMPE_INT_STA, 0xFF); // reset all ints
        latestMod = time(NULL);
    }
}

void renderWidgets(time_t now) {
    for (int i = 0; widgets[i]; i++) {
        widgets[i]->render(now);
    }
}

void reconnect() {
    // Wifi
    while (WiFi.status() != WL_CONNECTED) {
        digitalWrite(BACKLIGHT, HIGH);
        delay(1000);
        Serial.println(F("reconnecting to wifi..."));
        time_t now(time(NULL));
        renderWidgets(now);
    }

    // MQTT
    while (!client.connected()) {
        time_t now(time(NULL));
        digitalWrite(BACKLIGHT, HIGH);
        renderWidgets(now);
        showConnectionState();
        Serial.print(F("Attempting MQTT connection..."));
        // Attempt to connect
        if (client.connect("workshop-display", mqttUser, mqttAuth)) {
            Serial.println(F("connected"));
            client.subscribe(aqiTopic);
        } else {
            Serial.print(F("failed, rc="));
            Serial.print(client.lastError());
            Serial.println(F(" try again in 5 seconds"));
            delay(5000);
        }
    }
    showConnectionState();
}

void showStatusMessage() {
    static bool waiting(false);

    auto conned = client.connected();
    bool hasSomeData = aqiWidget.hasValue();

    tft.setTextSize(3);
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);

    if (conned && !hasSomeData) {
        if (waiting) {
            return;
        }
        statusMessage("waiting for data");
        waiting = true;
    } else if(!conned) {
        showConnectionState();
        waiting = false;
    } else if (hasSomeData) {
        statusMessage("");
        waiting=false;
    }
}

void loop() {
    ArduinoOTA.handle();

    if (!client.connected()) {
        reconnect();
    }
    client.loop();
    showStatusMessage();
    checkTouch();

    time_t now(time(NULL));
    renderWidgets(now);

    auto dtime = batteryWidget.reading() > 4 ? (3600*4) : 60;

    digitalWrite(BACKLIGHT, (difftime(now, latestMod) > dtime) ? LOW : HIGH);
}
