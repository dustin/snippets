/**
 *****************************************************************************
 * @file       NeoPixel_Transceiver.ino
 * @author     ernieift, main code, Copyright (C) 2014
 +             cGiesen, selftest
 * @brief      protocol converter to use rgb led stripes at async serial ports
 * @see        The GNU Public License (GPL) Version 3
 *
 *****************************************************************************/
/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public Licenseb
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

/* pinout used on ATtiny85
 * pin 1 -
 * pin 2 RX comes from host
 * pin 3 TX (not used at yet)
 * pin 4 GND
 * pin 5 DOUT goes to LED stripe
 * pin 6 -
 * pin 7 -
 * pin 8 VCC
 */

#include <SoftwareSerial.h>
#include <WS2812.h>
#include "msp.h"

/* global definition */
#define TIMEMAX 3

/* Serial definition */
#define RXPIN 3
#define TXPIN 4
#define BAUDRATE 19200
SoftwareSerial ser(RXPIN, TXPIN);

#define LEDPIN 0
#define LEDMAX 2
WS2812 LED(LEDMAX);

MSP msp;
bool armed(false);

void (*mode)()(mode_disarmed);

// Some useful lights
cRGB redLight{0, 0, 0};
cRGB lightOff{0, 0, 0};
cRGB blinkLight{0, 0, 0};

void setup() {
  ser.begin(BAUDRATE);
  LED.setOutput(LEDPIN);

  msp.rcCallback = rcCb;
  msp.statusCallback = stCb;

  redLight.r = 127;
  blinkLight.b = 127;

  blackout();
  LED.sync();
}

static void stCb(MSPStatus *status) {
    if (armed != msp.isArmed()) {
        mode = msp.isArmed() ? mode_idle : mode_disarmed;
    }
    armed = msp.isArmed();
}

int speed(0);
int blinkRate(0);

static void rcCb(uint16_t *rc_chans) {
    if (!armed) {
        return;
    }

    if (rc_chans[CHAN_ROLL] < 1300 || rc_chans[CHAN_YAW] < 1300) {
        if (mode != mode_left_blinker) {
            blackout();
        }
        blinkRate = map(min(rc_chans[CHAN_ROLL], rc_chans[CHAN_YAW]), 1300, 1000, 500, 150);
        mode = mode_left_blinker;
    } else if (rc_chans[CHAN_ROLL] > 1700 || rc_chans[CHAN_YAW] > 1700) {
        if (mode != mode_right_blinker) {
            blackout();
        }
        mode = mode_right_blinker;
        blinkRate = map(max(rc_chans[CHAN_ROLL], rc_chans[CHAN_YAW]), 1700, 2000, 500, 150);
    } else {
        mode = mode_speed;
        speed = map(constrain(rc_chans[CHAN_THROTTLE], 1000, 2000), 1000, 2000, 0, 100);
    }
}

unsigned long lastUpdate = 0;

void loop() {
    while (ser.available()) {
        msp.feed(ser.read());
    }
    mode();
    LED.sync();

    unsigned long now = millis();
    if (now - lastUpdate > 0 && now - lastUpdate < 10) {
        delay(now - lastUpdate);
    }
}

//
// Light Modes
//

void mode_disarmed() {
    cRGB value{0, 0, 0};
    value.r = 16;
    for (int i = 0; i < LEDMAX; i++) {
        LED.set_crgb_at(i, value);
    }
}

void blackout() {
    for (int i = 0; i < LEDMAX; i++) {
        LED.set_crgb_at(i, lightOff);
    }
}

void mode_idle() {
    blackout();
}

unsigned int blinkI(0);
unsigned long blinkNext(0);

void perform_blinker(int blink) {
    unsigned long now = millis();
    if (blinkNext > now) {
        return;
    }

    blinkNext = now + blinkRate;

    LED.set_crgb_at(blink, blinkI%2 == 0 ? blinkLight : lightOff);
    blinkI++;
}

void mode_left_blinker() {
    perform_blinker(1);
}

void mode_right_blinker() {
    perform_blinker(0);
}

void mode_speed() {
    cRGB value{0, 0, 0};
    value.r = map(speed, 0, 100, 0, 127);
    value.g = map(speed, 0, 100, 127, 0);

    for (int i = 0; i < LEDMAX; i++) {
        LED.set_crgb_at(i, value);
    }
}
