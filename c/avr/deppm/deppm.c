#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/sleep.h>
#include <util/atomic.h>

#define PPM_PIN PB0
#define PINMIN  PB1
#define PINMAX  PB4

static volatile uint8_t current = 0;
static volatile bool up = false;
static volatile bool skip = false;

ISR(PCINT0_vect) {
    if (skip || (current > PINMAX)) {
        TCNT0 = 0;
        skip = false;
        return;
    }

    if (up) {
        PORTB &= ~_BV(current);
        current++;
        up = false;
    } else {
        PORTB |= _BV(current);
        up = true;
    }
    TCNT0 = 0;
}


ISR(TIMER0_COMPA_vect) {
    // Shut down all the outputs and prepare for the nex
    PORTB = 0;
    current = PINMIN;
    up = false;
    skip = true;
}

int main() {
    DDRB = ~_BV(PPM_PIN);

    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        // Watch for pin.
        GIMSK |= _BV(PCIE);
        PCMSK |= _BV(PCINT0);

        TCCR0B |= _BV(CS02);
        OCR0A = 80; // about 2.6ms
        TIMSK |= _BV(OCIE0A);
        TCNT0 = 0;
    }

    set_sleep_mode(SLEEP_MODE_IDLE);
    for (;;) {
        sleep_mode();
    }
}

