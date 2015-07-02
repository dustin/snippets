#define PPM_PIN A3
#define OUT_PIN A1
#define BUTTON_PIN 0

// microseconds
#define INPUT_TIMEOUT 1000 * 1000

// milliseconds
#define BUTTON_HOLD 1000

bool failed = false;
int misses = 0;

void (*mode)() = enabled;

void setup() {
    pinMode(OUT_PIN, OUTPUT);
    pinMode(BUTTON_PIN, INPUT_PULLUP);
    digitalWrite(OUT_PIN, LOW);
}

void loop() {
    mode();
}

void disabled() {
    digitalWrite(OUT_PIN, LOW);
}

void enabled() {
    if (pulseIn(PPM_PIN, HIGH, INPUT_TIMEOUT / 10) == 0) {
        if (++misses >= 10) {
            failed = true;
        }
    } else {
        misses = 0;
        failed = false;
    }
    digitalWrite(OUT_PIN, failed ? HIGH : LOW);

    for (unsigned long start = millis(); digitalRead(BUTTON_PIN) == LOW;) {
        if (millis() - start > BUTTON_HOLD) {
            mode = disabled;

            // Maybe like, a beep or something?
            digitalWrite(OUT_PIN, HIGH);
            delay(250);
            digitalWrite(OUT_PIN, LOW);
            break;
        }
    }
}
