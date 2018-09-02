#include <CircularBuffer.h>

#include <WiFi.h>
#include <PubSubClient.h>

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

CircularBuffer<String, 5> errors;

void setup() {
    Serial.begin(115200);
    setupDisplay();
    setupWifi();
    setupMQTT();
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

    uint8_t i = 0;
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

bool hasShownStuff(false);

void maybeClear() {
    if (!hasShownStuff) {
        Serial.println("clearing screen to show stuff");
        tft.fillScreen(ILI9341_BLACK);
        hasShownStuff = true;
    }
    tft.setCursor(0, 0);
    tft.println(WiFi.localIP());
}

#define READING_ROW 2

void displayTemp(float t) {
    maybeClear();
    if (t < tooCold) {
        tft.setTextColor(ILI9341_BLUE, ILI9341_BLACK);
    } else if (t > tooHot) {
        tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
    } else {
        tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
    }
    tft.setCursor(0, FONT_HEIGHT*3*READING_ROW);
    tft.print(t);
    tft.print("C");
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
}

void displayHumidity(float h) {
    maybeClear();
    tft.setCursor(FONT_WIDTH*3*10, FONT_HEIGHT*3*READING_ROW);
    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
    tft.print(h);
    tft.print("%");
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

    errors.push(s);

    // font size is 5x8, but *3 and there are about three lines of
    // that.  We want to draw below that, so say, fifth line down...
    uint16_t topy = FONT_HEIGHT*3*5;

    tft.fillRect(0, topy, 320, 240-topy, ILI9341_BLACK);

    tft.setCursor(0, topy);

    tft.setTextColor(ILI9341_RED, ILI9341_BLACK);
    tft.setTextSize(2);

    for (unsigned int i = errors.size(); i > 0; i--) {
        tft.println(errors[i-1]);
    }

    tft.setTextColor(BASE_COLOR, ILI9341_BLACK);
    tft.setTextSize(3);
}

void callback(char* topic, byte* payload, unsigned int length) {
    Serial.print("Message arrived [");
    Serial.print(topic);
    Serial.print("] ");
    for (int i = 0; i < length; i++) {
        Serial.print((char)payload[i]);
    }
    Serial.println();

    if (strcmp(topic, workshopTemp) == 0) {
        char *end;
        displayTemp(strtof((char*)payload, &end));
    } else if (strcmp(topic, workshopHumidity) == 0) {
        char *end;
        displayHumidity(strtof((char*)payload, &end));
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

    if (conned && !hasShownStuff) {
        if (waiting) {
            return;
        }
        uint16_t topy = FONT_HEIGHT*3; // one row down at 3x
        tft.fillRect(0, topy, 320, FONT_HEIGHT*3, ILI9341_BLACK);
        tft.setCursor(0, topy);
        tft.println("waiting for data");
        waiting = true;
    } else if(!conned) {
        showConnectionState();
        waiting = false;
    }
}

void loop() {
    if (!client.connected()) {
        reconnect();
    }
    client.loop();
    showStatusMessage();
}
