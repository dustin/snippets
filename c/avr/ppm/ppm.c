#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <util/atomic.h>

#define PPM_PIN PB0

#define NCHAN 8
#define CHANSLATE(n) (n * 16)

#define T_0_3_MS (F_CPU * 0.0003)
#define MINUS_0_3_MS (0xffff-T_0_3_MS)

volatile uint16_t channels[] = {
    CHANSLATE(1000),
    CHANSLATE(1100),
    CHANSLATE(1200),
    CHANSLATE(1300),
    CHANSLATE(1400),
    CHANSLATE(1500),
    CHANSLATE(1600),
    CHANSLATE(1700),
};
volatile uint8_t chan = 0;
volatile bool sending = false;

// B hits first and raises the signal.
ISR(TIMER1_COMPB_vect) {
    PORTB |= _BV(PPM_PIN);
    if (++chan > NCHAN) {
        TCCR1B = 0; // disable timer
        sending = false;
    }
}

static void inline prepChannel() {
    PORTB &= ~_BV(PPM_PIN);
    OCR1B = T_0_3_MS;
    OCR1A = T_0_3_MS + channels[chan];
}

// Then A fires and lowers the signal and resets (via CTC) for the next run.
ISR(TIMER1_COMPA_vect) {
    prepChannel();
}

void emit() {
    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        // Turn up timer with /1 prescaler and CTC
        TCCR1B = _BV(WGM02) | _BV(CS10);
        sending = true;
        chan = 0;
        TCNT1 = 0;

        prepChannel();
    }
    while (sending) {}
}

int main() {
    // We're outputing ppm.
    DDRB |= _BV(PPM_PIN);

    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        TIMSK1 |= _BV(OCIE1A) | _BV(OCIE1B);
    }

    for (;;) {
        _delay_ms(3);
        emit();
    }
}
