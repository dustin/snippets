#include <FastLED.h>

#define outputPin 0
#define pwmPin A3
#define LEDCount 60

#define FRAMES_PER_SECOND 120
#define BRIGHTNESS        96

CRGB leds[LEDCount];

const int deadband = 5;
int hue;
unsigned long prevVal(1500);

void pulse();
void flash();
void emergency();
void rainbow();
void rainbowWithGlitter();
void confetti();
void sinelon();
void bpm();
void juggle();

void (*mode)()(pulse);

struct {
    unsigned long minVal;
    void (*mode)();
} modes[] = {
    {1900, flash},
    {1600, juggle},
    {1500, bpm},
    {1400, sinelon},
    {1300, confetti},
    {1200, rainbowWithGlitter},
    {1000, rainbow},
    {900, pulse},
    {0, emergency},
};

void setup() {
    delay(3000);
    FastLED.addLeds<NEOPIXEL, outputPin>(leds, LEDCount).setCorrection(TypicalLEDStrip);
    FastLED.setBrightness(BRIGHTNESS);
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

void rainbow() {
      fill_rainbow(leds, LEDCount, hue, 7);
}

void rainbowWithGlitter() {
    rainbow();
    if (random8() < 80) {
        leds[random16(LEDCount) ] += CRGB::White;
    }
}

void confetti() {
    fadeToBlackBy(leds, LEDCount, 10);
    int pos = random16(LEDCount);
    leds[pos] += CHSV(hue + random8(64), 200, 255);
}

void sinelon() {
    fadeToBlackBy(leds, LEDCount, 20);
    int pos = beatsin16(13,0,LEDCount);
    leds[pos] += CHSV( hue, 255, 192);
}

void bpm()
{
    uint8_t BeatsPerMinute = 62;
    CRGBPalette16 palette = PartyColors_p;
    uint8_t beat = beatsin8( BeatsPerMinute, 64, 255);
    for (int i = 0; i < LEDCount; i++) { //9948
        leds[i] = ColorFromPalette(palette, hue+(i*2), beat-hue+(i*10));
    }
}

void juggle() {
    fadeToBlackBy(leds, LEDCount, 20);
    byte dothue = 0;
    for (int i = 0; i < 8; i++) {
        leds[beatsin16(i+7,0,LEDCount)] |= CHSV(dothue, 200, 255);
    dothue += 32;
  }
}

void loop() {
    unsigned long val = pulseIn(pwmPin, HIGH, 50000);

    if (abs(val - prevVal) > deadband) {
        int i = 0;
        for (; val < modes[i].minVal; i++) {}
        mode = modes[i].mode;
    }

    mode();

    FastLED.show();
    FastLED.delay(1000/FRAMES_PER_SECOND);

    // do some periodic updates
    EVERY_N_MILLISECONDS( 20 ) { hue++; }

    prevVal = val;
}
