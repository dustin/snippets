#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/sleep.h>
#include <util/delay.h>
#include <stdbool.h>

#define PPM_PIN      PB0
#define OUT_PIN      PB1
#define BUTTON_PIN   PB2
#define SHUTDOWN_PIN PB3

// a bit under a second
#define BUTTON_OVERFLOWS 30

volatile uint8_t prevb = 0xFF;
volatile int overflows = 0;
volatile bool watching_button = false;

static inline void isr_svc_button() {
    if (watching_button) {
        watching_button = false;
        TIMSK &= ~_BV(TOIE0); // disable timer overflow
    } else {
        watching_button = true;
        TIMSK |= _BV(TOIE0); // enable overflow interupt
        TCNT0 = 0;
        overflows = 0;
    }
}

ISR(PCINT0_vect) {
    wdt_reset();

    uint8_t pinb = PINB;
    uint8_t vals = pinb ^ prevb;
    prevb = pinb;
    if (vals & _BV(PPM_PIN)) {
        PORTB &= ~_BV(OUT_PIN);
    }

    if (vals & _BV(BUTTON_PIN)) {
        isr_svc_button();
    }
}

ISR(WDT_vect) {
    wdt_reset();
    PORTB |= _BV(OUT_PIN);
}

ISR(TIM0_OVF_vect) {
    if (++overflows >= BUTTON_OVERFLOWS) {
        // We're done.  Disable the inputs so we can stop doing things.
        PCMSK &= ~(_BV(PCINT0) | _BV(PCINT2));

        // Indicator (maybe add a beep or something?)
        PORTB |= _BV(SHUTDOWN_PIN);

        // No more watchdog
        _WD_CONTROL_REG = 0;

        // Stop the timer.
        TIMSK &= ~_BV(TOIE0);
        watching_button = false;
    }
}

int main() {
    DDRB = _BV(OUT_PIN) | _BV(SHUTDOWN_PIN);
    PORTB |= _BV(BUTTON_PIN);

    cli();
    GIMSK |= _BV(PCIE);
    PCMSK |= _BV(PCINT0);

    // I use the timer later.  Set up 1024 prescaler.
    TCCR0B |= _BV(CS02) | _BV(CS00);  // 1024 prescaler

    // The watchdog timer is used for detecting failsafe state.
    wdt_reset();
    _WD_CONTROL_REG = _BV(_WD_CHANGE_BIT) | _BV(WDE);
    // Enable WDT Interrupt, and Set Timeout to ~1 seconds,
	_WD_CONTROL_REG = _BV(WDIE) | _BV(WDP2) | _BV(WDP1);
    sei();

    set_sleep_mode(SLEEP_MODE_PWR_SAVE);

    for (;;) {
        // just process interrupts
        sleep_mode();
    }
}
