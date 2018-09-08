#include <CircularBuffer.h>

#include <WiFi.h>
#include <WiFiMulti.h>
#include <PubSubClient.h>
#include <time.h>

#include <SPI.h>
#include <Adafruit_GFX.h>
#include <Adafruit_ILI9341.h>
#include <Adafruit_STMPE610.h>

const char* ssid = "XXXXXX";
const char* password = "XXXXXX";
const char* myname = "sensordisplay";

const char* mqttServer = "io.adafruit.com";
const char* mqttUser = "XXXXXX";
const char* mqttAuth = "XXXXXX";

const char* workshopTemp = "XXXXXX";
const char* workshopHumidity = "XXXXXX";
const char* errFeed = "XXXXXX";
const char* inTopic = "XXXXXX";

const float tooCold(10), tooHot(28);

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

WiFiMulti wifiMulti;

WiFiClient wiCli;
PubSubClient client(wiCli);

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

class ErrorWidget : public Widget {
public:

    ErrorWidget(int x, int y) : Widget(x, y), changed(false) {}

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

        for (unsigned int i = errors.size(); i > 0; i--) {
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
            Serial.print("Pruning error: ");
            Serial.print(errors.first().msg);
            Serial.print(": ");
            Serial.print(errors.first().ts);
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
    bool changed;
};

ErrorWidget errors(0, FONT_HEIGHT*3*5);

void age_str(char *buf, size_t buflen, time_t now, time_t ts) {
    double age = difftime(now, ts);
    int mins = age / 60;
    int secs = (int)age % 60;
    snprintf(buf, buflen, "-%d:%02d ", mins, secs);
}

class SensorValue : public Widget {
public:
    SensorValue(int xpos, int ypos, float l, float h, char c) : Widget(xpos, ypos),
                                                                v(NAN),
                                                                low(l), high(h),
                                                                sym(c), ts(0) {}

    void render(time_t now) {
        prune(now);

        if (staleness(now) < 1) {
            return;
        }

        tft.setCursor(x, y);
        tft.setTextSize(3);
        if (isnan(v)) {
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
            tft.fillRect(x, y, FONT_WIDTH*3*8, FONT_HEIGHT*3 + FONT_HEIGHT*2, ILI9341_BLACK);
        } else {
            if (v < low) {
                tft.setTextColor(ILI9341_BLUE, ILI9341_BLACK);
            } else if (v > high) {
                tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
            } else {
                tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
            }

            tft.print(v);
            tft.print(sym);

            // Show a timestamp on the next line.
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
            tft.setCursor(x, y+FONT_HEIGHT*3 + 2);
            struct tm tmts;
            localtime_r(&ts, &tmts);
            char buf[10];
            strftime(buf, sizeof(buf), "%H:%M", &tmts);
            tft.setTextSize(2);
            tft.print(buf);

            tft.setTextSize(1);
            tft.setCursor(x + FONT_WIDTH*2*6 + 4, y+FONT_HEIGHT*3 + 8);
            age_str(buf, sizeof(buf), now, ts);
            tft.print(buf);
        }
        modtime = now;
    }

    void update(float f, time_t when) {
        v = f;
        ts = when;
        modtime = 0;
    }

    void prune(time_t now) {
        double age = difftime(now, ts);
        if (hasValue() && age > OLDEST_READING) {
            v = NAN;
        }
    }

    bool hasValue() {
        return !isnan(v);
    }

    float v;
    float low, high;
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
        tft.print("wifi: ");
        tft.setCursor(x + FONT_WIDTH*2 + 13, y+4);

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
        Serial.print("Raw reading: ");
        Serial.println(r);
        float halfv = 3.30f * float(r) / 4096.0f;
        // auto halfv = r * (3.0 / 4096.0);
        auto v = halfv * 2;

        tft.setCursor(x, y);
        if (v > 3.8) {
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
        } else if (v > 3.6) {
            tft.setTextColor(ILI9341_YELLOW, ILI9341_BLACK);
        } else {
            tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
        }

        char buf[8];
        snprintf(buf, sizeof(buf), "%.2fV", v);
        tft.print(buf);
    }
};

SensorValue tempWidget(0, FONT_HEIGHT*3*READING_ROW, tooCold, tooHot, 'C');
SensorValue humidityWidget(FONT_WIDTH*3*HUMIDITY_COLUMN, FONT_HEIGHT*3*READING_ROW, 0, 100, '%');

TimeWidget timeWidget(320-(FONT_WIDTH*2*23), 240-FONT_HEIGHT*2);

WiFiWidget wifiWidget(0, 0);

BatteryWidget batteryWidget(0, 240-FONT_HEIGHT*2);

Widget* widgets[] = {&tempWidget, &humidityWidget, &timeWidget, &errors, &wifiWidget, &batteryWidget, nullptr};

void setup() {
    Serial.begin(115200);
    setupDisplay();
    setupWifi();
    setupTime();
    setupMQTT();

    pinMode(BATTERY_PIN, INPUT);
}

void statusMessage(String msg) {
    uint16_t topy = FONT_HEIGHT*3; // one row down at 3x
    tft.fillRect(0, topy, 320, FONT_HEIGHT*3, ILI9341_BLACK);
    tft.setCursor(0, topy);
    tft.print(msg);
 }

void setupTime() {
    configTime(0 /* tz */, 0 /* dst */, "pool.ntp.org");
    Serial.println("Waiting for time sync...");
    statusMessage("syncing time");
    while (time(nullptr) < 1535920965) {
        delay(10);
    }
    setenv("TZ", "PST8PDT7,M3.2.0/02:00:00,M11.1.0/02:00:00", 1);
    tzset();

    latestMod = time(NULL);
    Serial.print("Initial time: ");
    Serial.println(latestMod);
}


void setupDisplay() {
    pinMode(BACKLIGHT, OUTPUT);
    digitalWrite(BACKLIGHT, HIGH);

    tft.begin();
    tft.setRotation(3);
    tft.setTextSize(3);
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);

    // read diagnostics (optional but can help debug problems)
    uint8_t x = tft.readcommand8(ILI9341_RDMODE);
    Serial.print("Display Power Mode: 0x"); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDMADCTL);
    Serial.print("MADCTL Mode: 0x"); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDPIXFMT);
    Serial.print("Pixel Format: 0x"); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDIMGFMT);
    Serial.print("Image Format: 0x"); Serial.println(x, HEX);
    x = tft.readcommand8(ILI9341_RDSELFDIAG);
    Serial.print("Self Diagnostic: 0x"); Serial.println(x, HEX);

    tft.fillScreen(ILI9341_BLACK);

    if (! touch.begin()) {
        Serial.println(F("Touch screen not found"));
    }
}

void setupWifi() {
    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(LED_BUILTIN, LOW); // low is on, night is day

    wifiMulti.addAP("Spy Wireless IX", "monstercable");
    wifiMulti.addAP("Spy Wireless VIII", "monstercable");
    wifiMulti.addAP("GoogleGuest", "");
    wifiMulti.addAP("GoogleGuest-Legacy", "");

    tft.setCursor(0, 0);
    tft.println("connecting...");

    while (wifiMulti.run() != WL_CONNECTED) {
        digitalWrite(LED_BUILTIN, LOW);
        delay(500);
        digitalWrite(LED_BUILTIN, HIGH);
        delay(500);
    }

    Serial.println("IP address: ");
    Serial.println(WiFi.localIP());
    tft.fillScreen(ILI9341_BLACK);
    tft.setCursor(0, 0);
    tft.println(WiFi.localIP());

    digitalWrite(LED_BUILTIN, HIGH); // light's off when we're connected
}

void setupMQTT() {
    client.setServer(mqttServer, 1883);
    client.setCallback(callback);
}

void appendBytes(String &s, const byte* payload, unsigned int length) {
    s.reserve(length);
    for (int i = 0; i < length; i++) {
        s.concat((char)payload[i]);
    }
}

void displayErr(const byte* payload, unsigned int length) {
    String s;
    appendBytes(s, payload, length);
    errors.addError(s);
}

void callback(char* topic, byte* payload, unsigned int length) {
    Serial.print("Message arrived [");
    Serial.print(topic);
    Serial.print("] ");
    for (int i = 0; i < length; i++) {
        Serial.print((char)payload[i]);
    }
    Serial.println();

    time_t now(time(NULL));
    latestMod = now;
    if (strcmp(topic, workshopTemp) == 0) {
        char *end;
        tempWidget.update(strtof((char*)payload, &end), now);
    } else if (strcmp(topic, workshopHumidity) == 0) {
        char *end;
        humidityWidget.update(strtof((char*)payload, &end), now);
    } else if (strcmp(topic, errFeed) == 0) {
        displayErr(payload, length);
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
        switch (client.state()) {
        case MQTT_CONNECTION_TIMEOUT: tft.print("timeout"); break;
        case MQTT_CONNECTION_LOST: tft.print("conn lost"); break;
        case MQTT_CONNECT_FAILED: tft.print("conn failed"); break;
        case MQTT_DISCONNECTED: tft.print("disconnected"); break;
        case MQTT_CONNECTED: tft.print("connected"); break;
        case MQTT_CONNECT_BAD_PROTOCOL: tft.print("bad protocol"); break;
        case MQTT_CONNECT_BAD_CLIENT_ID: tft.print("bad client ID"); break;
        case MQTT_CONNECT_UNAVAILABLE: tft.print("unavailable"); break;
        case MQTT_CONNECT_BAD_CREDENTIALS: tft.print("bad creds"); break;
        case MQTT_CONNECT_UNAUTHORIZED: tft.print("unauthorized"); break;
        default:
            tft.print("unknown reason: ");
            tft.print(client.state());
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

void reconnect() {
    // Loop until we're reconnected
    while (!client.connected()) {
        digitalWrite(BACKLIGHT, HIGH);
        showConnectionState();
        Serial.print("Attempting MQTT connection...");
        // Attempt to connect
        if (client.connect("workshop-display", mqttUser, mqttAuth)) {
            Serial.println("connected");
            client.subscribe(inTopic);
            client.subscribe(workshopTemp);
            client.subscribe(workshopHumidity);
            client.subscribe(errFeed);
        } else {
            Serial.print("failed, rc=");
            Serial.print(client.state());
            Serial.println(" try again in 5 seconds");
            delay(5000);
        }
    }
    showConnectionState();
}

void showStatusMessage() {
    static bool waiting(false);

    auto conned = client.connected();
    bool hasSomeData = tempWidget.hasValue() || humidityWidget.hasValue();

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
    if (!client.connected()) {
        reconnect();
    }
    client.loop();
    showStatusMessage();
    checkTouch();

    time_t now(time(NULL));

    for (int i = 0; widgets[i]; i++) {
        widgets[i]->render(now);
    }

    digitalWrite(BACKLIGHT, difftime(now, latestMod) > 15 ? LOW : HIGH);
}
