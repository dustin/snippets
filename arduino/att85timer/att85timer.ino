#include <avr/interrupt.h>

void setup() {
    pinMode(1, OUTPUT);
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
    digitalWrite(1, HIGH);
}

ISR(TIMER0_OVF_vect) {
    digitalWrite(1, LOW);
}

void loop() {
    __asm__ __volatile__("sleep");
}
