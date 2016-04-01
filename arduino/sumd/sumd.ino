#include "sumd.h"

#define NCHAN 8

SUMD sumd(NCHAN);

void setup() {
    Serial.begin(115200);
}

void transmit() {
    Serial.write(sumd.bytes(), sumd.size());
}

void loop() {
    for (int i = 0; i < sumd.nchan(); i++) {
        sumd.setChannel(i, sumd.channel(i) + (i*2));
        if (sumd.channel(i) > 2000) {
            sumd.setChannel(i, 1000);
        }
    }
    transmit();
    delay(10);
}
