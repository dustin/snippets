#include <CircularBuffer.h>

#include <WiFi.h>
#include <PubSubClient.h>
#include <time.h>

#include <SPI.h>
#include <Adafruit_GFX.h>
#include <Adafruit_ILI9341.h>

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

WiFiClient wiCli;
PubSubClient client(wiCli);

Adafruit_ILI9341 tft = Adafruit_ILI9341(TFT_CS, TFT_DC);

#define BASE_COLOR ILI9341_GREEN

// How long (in seconds) errors can stay on the screen.
#define OLDEST_ERROR   (60*30)
#define OLDEST_READING (60*20)

#define READING_ROW 2
#define HUMIDITY_COLUMN 10

class TimedError {
public:
    TimedError(String m) : msg(m), ts(time(NULL)) {}
    TimedError() : TimedError("") {}

    String msg;
    time_t ts;
};

CircularBuffer<TimedError, 5> errors;

class Widget {
public:
    Widget(int xpos, int ypos) : x(xpos), y(ypos) {}

    virtual void render(time_t now) = 0;

    int x, y;
};

class SensorValue : public Widget {
public:
    SensorValue(int xpos, int ypos, float l, float h, char c) : Widget(xpos, ypos),
                                                                v(NAN),
                                                                low(l), high(h),
                                                                sym(c), ts(0) {}

    void render(time_t now) {
        tft.setCursor(x, y);
        tft.setTextSize(3);
        if (isnan(v)) {
            tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
            tft.fillRect(x, y, FONT_WIDTH*3*8, FONT_HEIGHT*3, ILI9341_BLACK);
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
            strftime(buf, sizeof(buf), "%H:%M:%S", &tmts);
            tft.setTextSize(2);
            tft.print(buf);
        }
    }

    void update(float f, time_t when) {
        v = f;
        ts = when;
        render(when);
    }

    void prune(time_t now) {
        double age = difftime(now, ts);
        if (hasValue() && age > OLDEST_READING) {
            v = NAN;
            render(now);
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
        tft.setTextColor(ILI9341_OLIVE, ILI9341_BLACK);
        tft.setCursor(x, y);
        struct tm tmts;
        localtime_r(&t, &tmts);
        char buf[24];
        strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%S", &tmts);
        tft.setTextSize(2);
        tft.print(buf);
    }
};

SensorValue tempWidget(0, FONT_HEIGHT*3*READING_ROW, tooCold, tooHot, 'C');
SensorValue humidityWidget(FONT_WIDTH*3*HUMIDITY_COLUMN, FONT_HEIGHT*3*READING_ROW, 0, 100, '%');

TimeWidget timeWidget(320-(FONT_WIDTH*2*23), 240-FONT_HEIGHT*2);

void setup() {
    Serial.begin(115200);
    setupDisplay();
    setupWifi();
    setupTime();
    setupMQTT();
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

    Serial.print("Initial time: ");
    Serial.println(time(NULL));
}


void setupDisplay() {
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
}

void setupWifi() {
    pinMode(LED_BUILTIN, OUTPUT);
    digitalWrite(LED_BUILTIN, LOW); // low is on, night is day

    WiFi.mode(WIFI_STA);
    // WiFi.hostname(myname);
    WiFi.begin(ssid, password);

    tft.setCursor(0, 0);
    tft.println("connecting...");

    while (WiFi.status() != WL_CONNECTED) {
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

int pruneErrors(time_t now) {
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
    return pruned;
}

void displayErrs() {
    // font size is 5x8, but *3 and there are about three lines of
    // that.  We want to draw below that, so say, fifth line down...
    uint16_t topy = FONT_HEIGHT*3*5;

    tft.fillRect(0, topy, 320, 240-topy, ILI9341_BLACK);

    tft.setCursor(0, topy);

    tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
    tft.setTextSize(2);

    for (unsigned int i = errors.size(); i > 0; i--) {
        tft.println(errors[i-1].msg);
    }

    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
    tft.setTextSize(3);
}

void displayErr(const byte* payload, unsigned int length) {
    String s;
    appendBytes(s, payload, length);

    TimedError e(s);
    errors.push(e);

    displayErrs();
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

void reconnect() {
    // Loop until we're reconnected
    while (!client.connected()) {
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

    static long lastPrune = 0;
    long now = millis();
    time_t nowt(time(NULL));
    if (now - lastPrune > 5000) {
        lastPrune = now;

        if (pruneErrors(nowt) > 0) {
            displayErrs();
        }
        tempWidget.prune(nowt);
        humidityWidget.prune(nowt);
    }
    timeWidget.render(nowt);
}
