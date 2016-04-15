#include <avr/io.h>
#include <avr/sleep.h>
#include <avr/wdt.h>
#include <avr/interrupt.h>

#define LEDS _BV(PB0)|_BV(PB1)

ISR(WDT_vect) {
    wdt_reset();
    PORTB ^= LEDS;
}

int main(void) {
    // Set PB0 and PB1 to output
    DDRB |= LEDS;

    cli();
    wdt_reset();
    _WD_CONTROL_REG = _BV(_WD_CHANGE_BIT) | _BV(WDE);
    // Enable WDT Interrupt, and Set Timeout to ~1 second
    _WD_CONTROL_REG = _BV(WDIE) | _BV(WDP2) | _BV(WDP1);
    sei();

    set_sleep_mode(SLEEP_MODE_PWR_DOWN);
    while (1) {
        sleep_mode();
    }
}
