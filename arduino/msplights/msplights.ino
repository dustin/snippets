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
#define BAUDRATE 38400
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
cRGB failLight{0, 0, 0};
cRGB speedLight{0, 0, 0};

void setup() {
  ser.begin(BAUDRATE);
  LED.setOutput(LEDPIN);

  msp.rcCallback = rcCb;
  msp.statusCallback = stCb;
  msp.clearInteresting();
  msp.setInteresting(MSP_BOXIDS);
  msp.setInteresting(MSP_STATUS);

  redLight.r = 127;
  blinkLight.b = 127;
  failLight.r = 127;

  blackout();
  LED.sync();
}

static void stCb(MSPStatus *status) {
    // Angle mode is only used for failsafe here.
    if (msp.status.flags & msp.boxes.stable) {
        armed = false;
        mode = mode_failsafe;
        return;
    }
    if (armed != msp.isArmed()) {
        mode = msp.isArmed() ? mode_idle : mode_disarmed;
    }
    armed = msp.isArmed();
    if (armed) {
        msp.setInteresting(MSP_RC);
    } else {
        msp.notInteresting(MSP_RC);
    }
}

int blinkRate(0);

static void rcCb(uint16_t *rc_chans) {
    if (!armed) {
        return;
    }

    if (rc_chans[CHAN_THROTTLE] < 1100) {
        // Don't go into blinker mode when arming or disarming
        mode = mode_speed;
        speedLight.r = 0;
        speedLight.g = 127;
    } else if (rc_chans[CHAN_ROLL] < 1300 || rc_chans[CHAN_YAW] < 1300) {
        blinkRate = map(min(rc_chans[CHAN_ROLL], rc_chans[CHAN_YAW]), 1300, 1000, 500, 150);
        mode = mode_left_blinker;
    } else if (rc_chans[CHAN_ROLL] > 1700 || rc_chans[CHAN_YAW] > 1700) {
        mode = mode_right_blinker;
        blinkRate = map(max(rc_chans[CHAN_ROLL], rc_chans[CHAN_YAW]), 1700, 2000, 500, 150);
    } else {
        mode = mode_speed;
        int speed constrain(rc_chans[CHAN_THROTTLE], 1000, 2000);
        speedLight.r = map(speed, 1000, 2000, 0, 127);
        speedLight.g = map(speed, 1000, 2000, 127, 0);
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

    LED.set_crgb_at(blink, blinkI%2 == 0 ? blinkLight : speedLight);
    blinkI++;
}

void mode_left_blinker() {
    LED.set_crgb_at(0, speedLight);
    perform_blinker(1);
}

void mode_right_blinker() {
    perform_blinker(0);
    LED.set_crgb_at(1, speedLight);
}

void mode_speed() {
    cRGB value{0, 0, 0};

    for (int i = 0; i < LEDMAX; i++) {
        LED.set_crgb_at(i, speedLight);
    }
}

void mode_failsafe() {
    unsigned long now = millis();
    if (blinkNext > now) {
        return;
    }

    blinkNext = now + 750;

    LED.set_crgb_at(0, blinkI%2 == 0 ? failLight : lightOff);
    LED.set_crgb_at(1, blinkI%2 == 0 ? failLight : lightOff);
    blinkI++;
}
