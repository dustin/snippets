#include <Servo.h>

Servo myservo;

void setup() {
    pinMode(13, OUTPUT);
    myservo.attach(9);  // attaches the servo on pin 9 to the servo object
}

void blink(int n) {
    for (int i = 0; i < n; i++) {
        digitalWrite(13, HIGH);
        delay(200);
        digitalWrite(13, LOW);
        delay(200);
    }
}

void loop() {
    for (int i = 0; i < 180; i+=10) {
        myservo.write(i);
        blink(i / 10);
        delay(min(3000, 3000 - (i/10) * 400));
    }
}
