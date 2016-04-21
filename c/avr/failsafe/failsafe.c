#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/wdt.h>
#include <avr/sleep.h>
#include <util/atomic.h>

#define PPM_PIN      PB0
#define OUT_PIN      PB1
#define BUTTON_PIN   PB2
#define SHUTDOWN_PIN PB3

// a bit under a second
#define BUTTON_OVERFLOWS 30

volatile uint8_t prevb = 0xFF;
volatile uint8_t overflows = 0;

// This part of the ISR(PCINT0_vect) runs when the button pin changes.
static inline void isr_svc_button() {
    if (PINB & _BV(BUTTON_PIN)) {
        TIMSK &= ~_BV(TOIE0); // button is up, disable timer overflow
    } else {
        TIMSK |= _BV(TOIE0);  // button is pressed, enable overflow interupt
        overflows = 0;
    }
}

// This is invoked for pin changes in either the case where signal is heard
// from the input, or the button is pressed or released.
ISR(PCINT0_vect) {
    wdt_reset();

    // Detect which pins are different.
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

// Watchdog is invoked when we've not seen pin changes in about a second.
ISR(WDT_vect, ISR_NAKED) {
    wdt_reset();
    PORTB |= _BV(OUT_PIN);
    reti();
}

// The timer starts running when the button is pressed.  Since we've only got
// 8-bit timers on an attiny85, we just count how many times this overflows
// (which is about every 32ms at 8MHz).  The timer will stop if the button is
// released (and reset if it's pressed again).
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
    }
}

int main() {
    // Setup outputs
    DDRB = _BV(OUT_PIN) | _BV(SHUTDOWN_PIN);
    // Pull-up button input with the button completing the circuit to ground.
    PORTB |= _BV(BUTTON_PIN);

    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        // Prepare the pin change interrupts.
        GIMSK |= _BV(PCIE);
        PCMSK |= _BV(PCINT0) | _BV(PCINT2);

        // I will use the timer later if someone presses a button.
        // Set up 1024 prescaler.
        TCCR0B |= _BV(CS02) | _BV(CS00);

        // The watchdog timer is used for detecting failsafe state.
        wdt_reset();
        _WD_CONTROL_REG = _BV(_WD_CHANGE_BIT) | _BV(WDE);
        // Enable WDT Interrupt, and Set Timeout to ~1 seconds,
        _WD_CONTROL_REG = _BV(WDIE) | _BV(WDP2) | _BV(WDP1);
    }

    set_sleep_mode(SLEEP_MODE_PWR_SAVE);

    for (;;) {
        // just process interrupts
        sleep_mode();
    }
}
