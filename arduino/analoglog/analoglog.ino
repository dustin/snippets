#include <Arduino.h>

#include <ESP8266WiFi.h>
#include <ESP8266WiFiMulti.h>

#include <ESP8266HTTPClient.h>

ESP8266WiFiMulti WiFiMulti;

const char* target = "http://192.168.1.67:8675/sensor/test";

void setup() {
    WiFiMulti.addAP("", "");
}

void loop() {
    if((WiFiMulti.run() == WL_CONNECTED)) {
        HTTPClient http;
        http.begin(target);

        // start connection and send HTTP header
        int httpCode = http.POST(String(analogRead(A0), DEC));

        // httpCode will be negative on error
        if(httpCode > 0) {
            // HTTP header has been send and Server response header has been handled
            // file found at server
            if(httpCode == HTTP_CODE_OK) {
                String payload = http.getString();
                // USE_SERIAL.println(payload);
            }
        } else {
            // USE_SERIAL.printf("[HTTP] GET... failed, error: %s\n", http.errorToString(httpCode).c_str());
        }

        http.end();
    }

    delay(60000);
}
