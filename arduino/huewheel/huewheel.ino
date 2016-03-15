/* pinout used on ATtiny85
 * pin 1 NC
 * pin 2 PWM Input
 * pin 3 NC
 * pin 4 GND
 * pin 5 DOUT goes to LED stripe
 * pin 6 NC
 * pin 7 NC
 * pin 8 VCC
 */

#include <EEPROM.h>
#include <FastLED.h>

#define LEDPIN 0
#define LEDMAX 100

#define pwmPin A3

bool dirty(false);
const int deadband(7);
const long valueTooLow(800);
const long defaultLow(1000);
const long defaultHigh(1800);
const long valueTooHigh(3000);
unsigned long prevVal(1500);

unsigned long blackVal(1010);
unsigned long lowest(defaultLow);
unsigned long highest(defaultHigh);
unsigned long nextWrite(0);

void setup() {
    FastLED.addLeds<NEOPIXEL, LEDPIN>(NULL, LEDMAX);

    pinMode(pwmPin, INPUT);
    loadEEProm();
}

void computeBlackVal() {
    blackVal = lowest + (0.02 * (highest - lowest));
}

void loadEEProm() {
    const int pos = 0;
    lowest = (EEPROM.read(pos) << 8) | EEPROM.read(pos + 1);
    highest = (EEPROM.read(pos + 2) << 8) | EEPROM.read(pos + 3);
    if (lowest < valueTooLow || lowest > valueTooHigh) {
        lowest = defaultLow;
    }
    if (highest > valueTooHigh || highest < valueTooLow) {
        highest = defaultHigh;
    }
    computeBlackVal();
}

void writeEEProm() {
    if (!dirty) {
        return;
    }

    int pos = 0;
    EEPROM.write(pos++, lowest >> 8);
    EEPROM.write(pos++, lowest & 0xff);
    EEPROM.write(pos++, highest >> 8);
    EEPROM.write(pos++, highest & 0xff);

    dirty = false;
}

void rangeCheck(unsigned long val) {
    if (val < valueTooLow || val > valueTooHigh) {
        if (val == 0) {
            lowest = defaultLow;
            highest = defaultHigh;
        }
        return;
    }
    if (val < lowest || val > highest) {
        lowest = min(val, lowest);
        highest = max(val, highest);
        computeBlackVal();
        dirty = true;
    }
}

CRGB color;

void loop() {
    unsigned long val = pulseIn(pwmPin, HIGH, 3000000);
    rangeCheck(val);
    if (abs(val - prevVal) > deadband) {
        prevVal = val;
        if (val > blackVal) {
            color.setHue(map(val, blackVal, highest, 0, 255));
        } else {
            color = CRGB::Black;
        }
        FastLED.showColor(color);
    }
    delay(10);

    if (dirty && millis() > nextWrite) {
        writeEEProm();
        nextWrite = millis() + 10000;
    }
}
