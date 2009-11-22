#include <Servo.h>

#define NREADINGS 7
#define DEBUG 0

int servoId = 3;
int potId = 0;
int thermistorId = 1;
int theLight = 13;

int readings[NREADINGS];
int previousReading = 0;

Servo s;

void setup() {
#if DEBUG
    Serial.begin(9600);
#endif /* DEBUG */
    for(int i = 0; i < NREADINGS; i++) {
        readings[i] = -1;
    }
    s.attach(servoId);
}

int computeSmoothedReading() {
    int cnt = 0;
    int sum = 0;

    for(int i = 0; i < NREADINGS; i++) {
        if(readings[i] >= 0) {
            cnt++;
            sum += readings[i];
        }
    }

    int avg = sum / cnt;
    if (abs(avg - previousReading) > 1) {
        previousReading = avg;
    }

    return previousReading;
}

int angle() {
    // The reading is scaled beyond the range of the servo so it's
    // more sensitive.
    for(int i = 0; i< NREADINGS - 1; i++) {
        readings[i] = readings[i+1];
    }
    int reading = map(analogRead(thermistorId), 0, 1023, 720, 0);
    readings[NREADINGS-1] = reading;

    int smoothedReading = computeSmoothedReading();

    // The potentiometer is used for calibrating the reading
    int range = map(analogRead(potId), 0, 1023, -720, 720);

    // Combining the reading and the range, we get the current position
    int rv = constrain(smoothedReading + range, 0, 180);

#if DEBUG
    Serial.print(reading, DEC);
    Serial.print("(");
    Serial.print(smoothedReading, DEC);
    Serial.print(")/");
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
