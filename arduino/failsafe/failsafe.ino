#define PPM_PIN A3
#define OUT_PIN A1

// microseconds
#define INPUT_TIMEOUT 1000 * 1000

void setup() {
    pinMode(OUT_PIN, OUTPUT);
    digitalWrite(OUT_PIN, LOW);
}

void loop() {
    digitalWrite(OUT_PIN, pulseIn(PPM_PIN, HIGH, INPUT_TIMEOUT) == 0 ? HIGH : LOW);
}
