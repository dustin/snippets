#include <avr/interrupt.h>

#define LEDS (_BV(PB0) | _BV(PB1));

void setup() {
    DDRB |= LEDS;
    cli();
    TCCR0A = 0;
    TCCR0B = 0;
    TCNT0 = 0;
    OCR0A = 127;
    TCCR0B |= ((1 << CS00) | (1 << CS02)); // 1024 prescaler
    TIMSK |= (1 << OCIE0A) | (1 << TOIE0);
    sei();
}

ISR(TIMER0_COMPA_vect) {
    OCR0A--;
    PORTB |= LEDS;
}

ISR(TIMER0_OVF_vect) {
    PORTB |= ~LEDS;
}

void loop() {
    __asm__ __volatile__("sleep");
}
