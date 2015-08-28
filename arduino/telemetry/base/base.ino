#include <JeeLib.h>
#include <avr/sleep.h>
#include "longtime.h"

const int THIS_ID(1);
const int LED_PORT(9);

const long MIN_REPORT_FREQ(900000);

/*
 Output:

 - Normal readings:

     < NODE PORT READING LOW-HIGH SEQ TIME_SINCE_PREV

 - Info messages

     # text

 - Exceptional messages

     * MS_SINCE_LAST_HEARD

 */

// The JeeLink LED is backwards.  I don't know why.
#define JEE_LED_ON 0
#define JEE_LED_OFF 1

typedef struct {
    int reading;
    int high;
    int low;
    byte port;
    byte seq;
} data_t;

static bool shouldSend(false);
static unsigned long lastHeard(0);
static unsigned long offAfter(0);

static LongTimer lastHeardTimer;

typedef float(decoder)(int);

float decodeTemp(int reading) {
    return ((reading * (3300.0 / 1024.0)) - 500.0) / 10.0;
}

typedef struct {
    int host;
    int port;
    decoder *dec;
} hpdecoder;

hpdecoder decoders[] = {
    {9, 0, decodeTemp},
    {0, 0, NULL},
};

ISR(WDT_vect) { Sleepy::watchdogEvent(); }

void setup () {
    pinMode(LED_PORT, OUTPUT);
    digitalWrite(LED_PORT, JEE_LED_OFF);

    Serial.begin(57600);
    rf12_initialize(THIS_ID, RF12_433MHZ, 4);
    Serial.print("# Initialized ");
    Serial.println(THIS_ID);

    lastHeardTimer.set(MIN_REPORT_FREQ);
}

float decode(int host, int port, int reading) {
    for (int i = 0; decoders[i].dec != NULL; i++) {
        if (decoders[i].host == host && decoders[i].port == port) {
            return decoders[i].dec(reading);
        }
    }
    return (float)reading;
}

void loop () {
    data_t data;

    if (rf12_recvDone() && rf12_crc == 0 && rf12_len == sizeof(data)) {

        data = *((data_t*)rf12_data);

        digitalWrite(LED_PORT, JEE_LED_ON);
        delay(250);
        digitalWrite(LED_PORT, JEE_LED_OFF);

        int sender = rf12_hdr & 0xf;

        Serial.print("< ");
        Serial.print(sender);
        Serial.print(" ");
        Serial.print(data.port);
        Serial.print(" ");
        Serial.flush();
        Serial.print(decode(sender, data.port, data.reading));
        Serial.print(" ");
        Serial.flush();
        Serial.print(decode(sender, data.port, data.low));
        Serial.print("-");
        Serial.flush();
        Serial.print(decode(sender, data.port, data.high));
        Serial.print(" ");
        Serial.flush();
        Serial.println(data.seq);
        Serial.flush();

        lastHeardTimer.set(MIN_REPORT_FREQ);
        lastHeard = millis();

        if (RF12_WANTS_ACK) {
            rf12_sendStart(RF12_ACK_REPLY, &data.seq, sizeof(data.seq));
            rf12_sendWait(1); // don't power down too soon
        }

        // power down for 2 seconds (multiple of 16 ms)
        rf12_sleep(RF12_SLEEP);
        Sleepy::loseSomeTime(2000);
        rf12_sleep(RF12_WAKEUP);
    } else {
        // switch into idle mode until the next interrupt
        set_sleep_mode(SLEEP_MODE_IDLE);
        sleep_mode();
    }

    if (lastHeardTimer.ready()) {
        lastHeardTimer.set(MIN_REPORT_FREQ);
        Serial.print("* ");
        Serial.flush();
        Serial.println(millis() - lastHeard);
    }
}
