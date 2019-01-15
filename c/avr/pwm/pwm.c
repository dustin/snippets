#include <stdbool.h>
#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <util/atomic.h>

int main() {
    ATOMIC_BLOCK(ATOMIC_FORCEON) {
        /*
        DDRB |= (1 << PORTB1); //set port B pin 1 as output

        // TCCR1A |= (1 << WGM10) | (1 << WGM11); //set fast PWM Mode 10 bit (0x03ff-TOP)
        TCCR1A |= (1 << WGM10) | (1 << WGM11); //set fast PWM Mode 10 bit (0x03ff-TOP)
        // TCCR1B |= (1 << WGM12); //set fast PWM Mode 10 bit (0x03ff-TOP)
        TCCR1A |= (1 << COM1A1); //enable OC1A output for PWM mode clearing on compare match
        TCCR1B |= (1 << CS00) | (1 << CS01); //set PWM prescaler
        */


        TCCR1A|=(1<<COM1A1)|(1<<COM1B1)|(1<<WGM11);
        TCCR1B|=(1<<WGM13)|(1<<WGM12)|(1<<CS11) | (1<<CS10);
        ICR1=2500;
        DDRB|=(1<<PB1);   //PWM Pins as Out
    }

    OCR1A = 250;
    for (;;) {
        _delay_ms(3);
    }
}
