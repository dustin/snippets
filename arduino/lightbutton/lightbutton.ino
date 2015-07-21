#include <Adafruit_NeoPixel.h>

#include "color.h"

#define INPIN 3

#define LEDPIN 0
#define LEDMAX 25

color colors[] = {
  {0, 0, 0},
  {255, 0, 0},
  {255, 127, 0},
  {255, 255, 0},
  {0, 255, 0},
  {0, 0, 255},
  {75, 0, 130},
  {143, 0, 255},
  {0, 0, 0},
};

int current_color = 0;

Adafruit_NeoPixel strip = Adafruit_NeoPixel(LEDMAX, LEDPIN, NEO_GRB + NEO_KHZ800);

void setup() {
  pinMode(INPIN, INPUT_PULLUP);
  strip.begin();
  strip.show();
}

long debounceTime = millis();

void loop() {
  if (debounceTime > millis()) {
    return;
  }

  while (digitalRead(INPIN) == HIGH);
  while (digitalRead(INPIN) == LOW);
  debounceTime = millis() + 50;

  color c = colors[current_color++];
  for (int i = 0; i < LEDMAX; i++) {
    strip.setPixelColor(i, c.r, c.g, c.b);
  }
  strip.show();
  
  if (c.r == 0 && c.g == 0 && c.b == 0) {
    current_color = 0;
  }
}

