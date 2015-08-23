#include <JeeLib.h>
#include <avr/sleep.h>

// #define DEBUG

#ifdef DEBUG
# define dprintln Serial.println
# define dprint Serial.print
# define dprintd(a, b) Serial.print(a, b)
# define dflush Serial.flush
#else
# define dprintln(x)
# define dprint(x)
# define dprintd(x, y)
# define dflush()
#endif

const int THIS_ID(9);
const int POLL_FREQ(10000);
const int XMIT_FREQ(60000);
const int NUM_PORTS(2);

static MilliTimer pollTimer, xmitTimer;
static unsigned long lastAck(0);

typedef struct {
    int reading;
    int high;
    int low;
    byte port;
    byte seq;
} data_t;

static data_t data[NUM_PORTS];
static bool shouldSend[NUM_PORTS];
static bool acked[NUM_PORTS];
static byte global_seq(0);

ISR(WDT_vect) { Sleepy::watchdogEvent(); }

static byte next_seq() {
    if (++global_seq > 250) {
        global_seq = 0;
    }
    return global_seq;
}

void setup () {
    pollTimer.set(POLL_FREQ);
    xmitTimer.set(XMIT_FREQ);

    // Port initialization
    for (int i = 0; i < NUM_PORTS; ++i) {
        data[i].reading = 0;
        data[i].high = 0;
        data[i].port = i;
        data[i].seq = next_seq();
        shouldSend[i] = false;
        acked[i] = true;
    }

#ifdef DEBUG
    Serial.begin(57600);
#endif

    rf12_initialize(THIS_ID, RF12_433MHZ, 4);

    dprint("Initialized ");
    dprintln(THIS_ID);
    dflush();
}

static bool shouldSendAny() {
    bool rv(false);
    for (int i = 0; i < NUM_PORTS; ++i) {
        rv |= shouldSend[i];
    }
    return rv;
}

static bool awaitingACKs() {
    bool rv(false);
    for (int i = 0; i < NUM_PORTS; ++i) {
      rv |= !acked[i];
    }
    return rv;
}

void loop () {
    if (rf12_recvDone() && rf12_crc == 0) {
        if (rf12_len == sizeof(byte)) {
            byte recv_seq(*rf12_data);

            for (int i = 0; i < NUM_PORTS; ++i) {
                if (data[i].seq == recv_seq) {
                    data[i].seq = next_seq();
                    data[i].low = data[i].reading;
                    data[i].high = data[i].reading;
                    acked[i] = true;
                }
            }
        } else {
            dprintln("Incorrect ACK response size.");
        }
        dprintln("Got an ACK.");
        lastAck = millis();
    }

    if (pollTimer.poll()) {
        dprint("Read: ");
        for (int i = 0; i < NUM_PORTS; ++i) {
            int r = analogRead(i);

            dprint(r);
            dprint(" ");
            dflush();
            if (r != data[i].reading) {
                data[i].reading = r;
                data[i].high = max(data[i].high, r);
                data[i].low = min(data[i].low, r);
                shouldSend[i] = true;
            }
            if (!acked[i]) {
                shouldSend[i] = true;
            }
            data[i].reading = r;
        }
        dprintln("");
        dflush();
        pollTimer.set(POLL_FREQ);
    }

    if (xmitTimer.poll()) {
        for (int i = 0; i < NUM_PORTS; ++i) {
            shouldSend[i] = true;
        }
    }

    if (shouldSendAny()) {
        bool saidIt = false;
        for (int i = 0; i < NUM_PORTS; ++i) {
            if (shouldSend[i] && rf12_canSend()) {
                if (!saidIt) {
                    dprint("Transmitting ");
                    saidIt = true;
                }

                rf12_sendStart(RF12_HDR_ACK, &data[i], sizeof(data[i]));

                dprint(data[i].port);
                dprint(":");
                dflush();
                dprint(data[i].reading);
                dprint("@");
                dprintd(data[i].seq, DEC);
                dflush();
                dprint(" ");

                shouldSend[i] = false;
                acked[i] = false;
            }
        }
        if (saidIt) {
            dprintln("");
            dflush();
        }

        xmitTimer.set(XMIT_FREQ);
    }

    if (!awaitingACKs()) {
        rf12_sleep(RF12_SLEEP);
        Sleepy::loseSomeTime(POLL_FREQ);
        rf12_sleep(RF12_WAKEUP);
    }
}
