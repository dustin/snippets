#include <avr/io.h>
#include <util/delay.h>

#define LED_on (PORTB |= (1<<PB0)|(1<<PB1))
#define LED_off (PORTB &= ~((1<<PB0)|(1<<PB1)))

int main(void) {
    // Set PB0 and PB1 to output
    DDRB |= (1<<PB0)|(1<<PB1);

    while (1) {
        LED_on;
        _delay_ms(1000);
        LED_off;
        _delay_ms(1000);
    }
}
