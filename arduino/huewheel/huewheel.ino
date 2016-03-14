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
#include <Adafruit_NeoPixel.h>

/* LED definition */
// Parameter 1 = number of pixels in strip
// Parameter 2 = Arduino pin number (most are valid)
// Parameter 3 = pixel type flags, add together as needed:
//   NEO_KHZ800  800 KHz bitstream (most NeoPixel products w/WS2812 LEDs)
//   NEO_KHZ400  400 KHz (classic 'v1' (not v2) FLORA pixels, WS2811 drivers)
//   NEO_GRB     Pixels are wired for GRB bitstream (most NeoPixel products)
//   NEO_RGB     Pixels are wired for RGB bitstream (v1 FLORA pixels, not v2)
#define LEDPIN 0
#define LEDMAX 100
Adafruit_NeoPixel strip = Adafruit_NeoPixel(LEDMAX, LEDPIN, NEO_GRB + NEO_KHZ800);

#define pwmPin A3

bool dirty = false;
const int deadband = 7;
const int valueTooLow = 500;
const int valueTooHigh = 3000;
unsigned long prevVal(1500);

unsigned long lowest = 1000;
unsigned long highest = 1800;
unsigned long nextWrite = 0;

void setup() {
  pinMode(pwmPin, INPUT);
  loadEEProm();
  strip.begin();
  strip.show(); // Initialize all pixels to 'off'
}

void loadEEProm() {
  const int pos = 0;
  lowest = (EEPROM.read(pos) << 8) | EEPROM.read(pos + 1);
  highest = (EEPROM.read(pos + 2) << 8) | EEPROM.read(pos + 3);
  if (lowest < valueTooLow || highest > valueTooHigh) {
    lowest = 6000;
  }
  if (lowest > valueTooHigh || highest < valueTooLow) {
    highest = 0;
  }
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
    return;
  }
  if (val < lowest || val > highest) {
    lowest = min(val, lowest);
    highest = max(val, highest);
    dirty = true;
  }
}

void hsvToRgb(double h, double s, double v, byte rgb[]) {
  double r, g, b;

  int i = int(h * 6);
  double f = h * 6 - i;
  double p = v * (1 - s);
  double q = v * (1 - f * s);
  double t = v * (1 - (1 - f) * s);

  switch (i % 6) {
    case 0: r = v, g = t, b = p; break;
    case 1: r = q, g = v, b = p; break;
    case 2: r = p, g = v, b = t; break;
    case 3: r = p, g = q, b = v; break;
    case 4: r = t, g = p, b = v; break;
    case 5: r = v, g = p, b = q; break;
  }

  rgb[0] = r * 255;
  rgb[1] = g * 255;
  rgb[2] = b * 255;
}



void loop() {
  byte rgb[] = {0, 0, 0};

  unsigned long val = pulseIn(pwmPin, HIGH, 50000);
  rangeCheck(val);
  if (abs(val - prevVal) > deadband) {
    prevVal = val;
    double hue = (double)map(val, lowest, highest, 0, 1000);
    hsvToRgb(hue / 1000, 1, 1, rgb);
    for (int i = 0; i < LEDMAX; i++) {
      strip.setPixelColor(i, rgb[0], rgb[1], rgb[2]);
    }
    strip.show();
  }
  delay(10);

  if (dirty && millis() > nextWrite) {
    writeEEProm();
    nextWrite = millis() + 10000;
  }
}
