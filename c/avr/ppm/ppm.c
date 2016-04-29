#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <util/atomic.h>

#define PPM_PIN PB0

#define NCHAN 8
#define CHANSLATE(n) (n * 16)

#define MINUS_0_3_MS (65536-4800)

volatile uint16_t channels[] = {
    CHANSLATE(1000),
    CHANSLATE(1200),
    CHANSLATE(1500),
    CHANSLATE(1700),

    CHANSLATE(2000),
    CHANSLATE(1700),
    CHANSLATE(1500),
    CHANSLATE(1200),
};
volatile uint8_t chan = 0;
volatile bool sending = false;

ISR(TIMER1_OVF_vect) {
    PORTB |= _BV(PPM_PIN);
    if (chan > NCHAN) {
        TIMSK1 &= ~(_BV(OCIE1A) | _BV(TOIE1));
        PORTB |= _BV(PPM_PIN);
        sending = false;
    } else {
        TIMSK1 |= _BV(OCIE1A);
    }
}

ISR(TIMER1_COMPA_vect) {
    PORTB &= ~_BV(PPM_PIN);
    TCNT1 = MINUS_0_3_MS;
    OCR1A = channels[chan++];
    TIMSK0 &= ~_BV(OCIE1A);
}

void emit() {
    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        chan = 0;
        TIMSK1 |= _BV(TOIE1);
        OCR1A = channels[chan];
        TCNT1 = MINUS_0_3_MS;
        PORTB &= ~_BV(PPM_PIN);
        sending = true;
    }
    while (sending) {}
}

int main() {
    // We're outputing ppm.
    DDRB |= _BV(PPM_PIN);

    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        // Set up /1 prescaler.
        TCCR1B |= _BV(CS10);
    }

    for (;;) {
        _delay_ms(3);
        emit();
    }
}
