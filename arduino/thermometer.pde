#include <Servo.h>

int servoId = 3;
int potId = 0;
int thermistorId = 1;
int theLight = 13;

#define DEBUG 0

Servo s;

void setup() {
#if DEBUG
    Serial.begin(9600);
#endif /* DEBUG */
    s.attach(servoId);
}

int angle() {
    // The reading is scaled beyond the range of the servo so it's
    // more sensitive.
    int reading = map(analogRead(thermistorId), 0, 1023, 360, 0);

    // The potentiometer is used for calibrating the reading
    int range = map(analogRead(potId), 0, 1023, -360, 360);

    // Combining the reading and the range, we get the current position
    int rv = constrain(reading + range, 0, 180);

#if DEBUG
    Serial.print(reading, DEC);
    Serial.print("/");
    Serial.print(range, DEC);
    Serial.print(" -> ");
    Serial.print(rv, DEC);
    Serial.println("");
#endif /* DEBUG */

    // We use the light to indicate whether we're within a reasonable
    // calibrated range for the analog display.  You could also just like,
    // Aim for 90°, but that'd be too easy.
    if (rv > 45 && rv < 135) {
        digitalWrite(theLight, HIGH);
    }
    else {
        digitalWrite(theLight, LOW);
    }

    return rv;
}

void loop() {
    // Move the servo to the computed angle.
    s.write(angle());
}
