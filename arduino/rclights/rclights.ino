#include <EEPROM.h>

#include "rclights.h"

const int deadBand = 2;
const int closeEnough = 20;
const int eepromTooLow = 500;
const int eepromTooHigh = 3000;

unsigned long nextWrite = 0;

sensor led1 = { A1, 0, 6000, 0, 0, false};
sensor led2 = { A2, 1, 6000, 0, 0, false};

void setup() {
  setupLed(&led1);
  setupLed(&led2);
  loadEEProm(&led1, 0);
  loadEEProm(&led2, 4);
}

void setupLed(sensor *led) {
  pinMode(led->in, INPUT);
  pinMode(led->out, OUTPUT);
}

void loadEEProm(sensor *led, int pos) {
  led->minValue = (EEPROM.read(pos) << 8) | EEPROM.read(pos + 1);
  led->maxValue = (EEPROM.read(pos + 2) << 8) | EEPROM.read(pos + 3);
  if (led->minValue < eepromTooLow || led->minValue > eepromTooHigh) {
    led->minValue = 6000;
  }
  if (led->maxValue > eepromTooHigh || led->maxValue < eepromTooLow) {
    led->maxValue = 0;
  }
}

void writeEEProm(sensor *led, int pos) {
  if (!led->dirty) {
    return;
  }

  EEPROM.write(pos++, led->minValue >> 8);
  EEPROM.write(pos++, led->minValue & 0xff);
  EEPROM.write(pos++, led->maxValue >> 8);
  EEPROM.write(pos++, led->maxValue & 0xff);
  
  led->dirty = false;
}

void rangeCheck(sensor *led, unsigned long val) {
  if (val < led->minValue || val > led->maxValue) {
    led->minValue = min(val, led->minValue);
    led->maxValue = max(val, led->maxValue);
    led->dirty = true;
  }
}

void updateLed(sensor* led) {
  unsigned long val = pulseIn(led->in, HIGH);
  while (val == 0) { // failsafe
    analogWrite(led->out, 255);
    delay(200);
    analogWrite(led->out, 0);
    val = pulseIn(led->in, HIGH);
  }

  rangeCheck(led, val);
  if (abs(val - led->lastValue) > deadBand) {
    if (val < led->minValue + closeEnough) {
      analogWrite(led->out, 0);
    } else {
      analogWrite(led->out, map(val, led->minValue, led->maxValue, 0, 255));
    }
    led->lastValue = val;
  }
}

void loop() {
  updateLed(&led1);
  updateLed(&led2);
  delay(100);
  
  if (millis() > nextWrite) {
    writeEEProm(&led1, 0);
    writeEEProm(&led2, 4);
    nextWrite = millis() + 10000;
  }
}
