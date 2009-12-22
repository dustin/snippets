#define THERM_PIN   0  // TMP36.

void setup() {
    Serial.begin(9600);
}

void loop() {
    int reading = analogRead(THERM_PIN);
    float voltage = reading * 5.0 / 1024;

    // converting from 10 mv per degree wit 500 mV offset
    float therm = (voltage - 0.5) * 100;

    Serial.print(therm);
    Serial.print(" ");
    Serial.println(millis());

    delay(10000);
}
