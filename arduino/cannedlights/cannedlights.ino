#include <WS2812.h>

#define outputPin 0
#define pwmPin A3
#define LEDCount 2

WS2812 LED(LEDCount);

const int deadband = 5;
unsigned long prevVal(1500);

void (*mode)();

struct {
    unsigned long minVal;
    void (*mode)();
} modes[] = {
    {1500, flash},
    {900, pulse},
    {0, emergency},
};

void setup() {
    pinMode(pwmPin, INPUT);
    LED.setOutput(outputPin);

    mode = pulse;
}

byte pulseIntensity(0);
byte pulseSign(1);

void pulse() {
    if (pulseSign) {
        pulseIntensity++;
        if (pulseIntensity == 255) {
            pulseSign = 0;
        }
    }
    else {
        pulseIntensity--;
        if (pulseIntensity == 0) {
            pulseSign = 1;
        }
    }

    for (int i = 0; i < LEDCount; i++) {
        cRGB value;
        if ((i % 3) == 0) { 
            value.b = 0;
            value.g = 0;
            value.r = pulseIntensity;
            LED.set_crgb_at(i, value);
        } else if ((i % 3) == 1) {
            value.b = 0;
            value.g = pulseIntensity;
            value.r = 0;
            LED.set_crgb_at(i, value);
        } else  {
            value.b = pulseIntensity;
            value.g = 0;
            value.r = 0;
            LED.set_crgb_at(i, value);
        }
    }

    LED.sync();
    delay(10);
}

unsigned int flashI = 0;

void flash() {
    for (int i = 0; i < LEDCount; i++) {
        cRGB value{0, 0, 0};
        if ((i+flashI) % 2) {
            value.r = 127;
        } else {
            value.b = 127;
        }
        LED.set_crgb_at(i, value);
    }
    flashI++;
    LED.sync();
    delay(500);
}

void emergency() {
    cRGB value{0, 0, 0};
    value.r = 127;
    for (int i = 0; i < LEDCount; i++) {
        LED.set_crgb_at(i, value);
    }
    LED.sync();
    delay(250);
    value.r = 0;
    for (int i = 0; i < LEDCount; i++) {
        LED.set_crgb_at(i, value);
    }
    LED.sync();
    delay(500);
}

void loop() {
    unsigned long val = pulseIn(pwmPin, HIGH, 5000);

    if (abs(val - prevVal) > deadband) {
        int i = 0;
        for (; val < modes[i].minVal; i++) {}
        mode = modes[i].mode;
    }

    mode();

    prevVal = val;
}
