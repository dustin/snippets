#include <FastLED.h>

#define outputPin 0
#define pwmPin A3
#define LEDCount 2

CRGB leds[LEDCount];

const int deadband = 5;
unsigned long prevVal(1500);

void pulse();
void flash();
void emergency();

void (*mode)()(pulse);

struct {
    unsigned long minVal;
    void (*mode)();
} modes[] = {
    {1500, flash},
    {900, pulse},
    {0, emergency},
};

void setup() {
    FastLED.addLeds<NEOPIXEL, outputPin>(leds, LEDCount);
    pinMode(pwmPin, INPUT);
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
        leds[i].red = 0;
        leds[i].green = 0;
        leds[i].blue = 0;
        if ((i % 3) == 0) {
            leds[i].red = pulseIntensity;
        } else if ((i % 3) == 1) {
            leds[i].green = pulseIntensity;
        } else  {
            leds[i].blue = pulseIntensity;
        }
    }

    FastLED.show();
    delay(30);
}

unsigned int flashI = 0;

void flash() {
    for (int i = 0; i < LEDCount; i++) {
        if ((i+flashI) % 2) {
            leds[i].red = 127;
            leds[i].blue = 0;
        } else {
            leds[i].blue = 127;
            leds[i].red = 0;
        }
    }
    flashI++;
    FastLED.show();
    delay(500);
}

void emergency() {
    FastLED.showColor(CRGB::Red);
    delay(250);
    FastLED.showColor(CRGB::Black);
    delay(500);
}

void loop() {
    unsigned long val = pulseIn(pwmPin, HIGH, 50000);

    if (abs(val - prevVal) > deadband) {
        int i = 0;
        for (; val < modes[i].minVal; i++) {}
        mode = modes[i].mode;
    }

    mode();

    prevVal = val;
}
