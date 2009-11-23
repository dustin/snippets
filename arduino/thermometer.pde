#include <Servo.h>

#define DECAY_FACTOR 3
#define DEBUG 0

#define SERVO_ID 3
#define POT_ID 0
#define THERMISTOR_ID 1
#define THE_LIGHT 13

int decayAvg = -1;

#if DEBUG
int prevValue = -1;
#endif /* DEBUG */

Servo s;

void setup() {
#if DEBUG
    Serial.begin(9600);
#endif /* DEBUG */
    s.attach(SERVO_ID);
}

int computeSmoothedReading(int newReading) {

    if(decayAvg == -1) {
        decayAvg = newReading;
    } else {
        decayAvg = ((decayAvg * DECAY_FACTOR) + newReading) / (DECAY_FACTOR + 1);
    }

    return (int)decayAvg;
}

int angle() {
    // The reading is scaled beyond the range of the servo so it's
    // more sensitive.
    int reading = map(analogRead(THERMISTOR_ID), 0, 1023, 720, 0);

    // The potentiometer is used for calibrating the reading
    int range = map(analogRead(POT_ID), 0, 1023, -720, 720);

    // Combining the reading and the range, we get the current position
    int rv = computeSmoothedReading(constrain(reading + range, 0, 180));

#if DEBUG
    if (rv != prevValue) {
        Serial.print(reading, DEC);
        Serial.print("/");
        Serial.print(range, DEC);
        Serial.print(" -> ");
        Serial.print(rv, DEC);
        Serial.println("");
        prevValue = rv;
    }
#endif /* DEBUG */

    // We use the light to indicate whether we're within a reasonable
    // calibrated range for the analog display.  You could also just like,
    // Aim for 90°, but that'd be too easy.
    if (rv > 45 && rv < 135) {
        digitalWrite(THE_LIGHT, HIGH);
    }
    else {
        digitalWrite(THE_LIGHT, LOW);
    }

    return rv;
}

void loop() {
    // Move the servo to the computed angle.
    s.write(angle());
}
