__SP_H__ = 0x3e
__SP_L__ = 0x3d
__SREG__ = 0x3f
PINB = 0x16
DDRB = 0x17
PORTB = 0x18
GIMSK = 0x3b
TIMSK = 0x39
TCNT0 = 0x32
TCCR0B = 0x33
OCR0A = 0x29
MCUSR = 0x34
WDTCR = 0x21
PCMSK = 0x15
MCUCR = 0x35
PPM_PIN = 0
PINMIN = 1
PINMAX = 5    
tmp1 = 24
tmp2 = 25
__zero_reg__ = 1
current = 18
up = 19

    .text

// Pin change ISR
.global __vector_2
    .type   __vector_2, @function
__vector_2:
    mov tmp1,current
    andi tmp1,lo8(0b11110)
    breq proceed

earlyExit:
    lsl current
    rjmp clearTimer

proceed:
    tst up
    breq isdown

    // we're up
    dec up
    out PORTB,__zero_reg__
    lsl current
    rjmp clearTimer

isdown:
    out PORTB,current
    inc up

clearTimer:
    out TCNT0,__zero_reg__
donePin:
    reti

.global __vector_10
    .type   __vector_10, @function

// Timer ISR
__vector_10:
    out PORTB,__zero_reg__
    ldi current,lo8(1)
    clr up
    reti

    .size   __vector_10, .-__vector_10

.section    .text.startup,"ax",@progbits
.global main
.type   main, @function

main:
    clr __zero_reg__

    ldi tmp1,lo8(0b00111110) // not the ppm pin
    out DDRB,tmp1

    cli
    // Set up pin change interrupt
    in tmp1,GIMSK
    ori tmp1,lo8(32)
    out GIMSK,r24
    sbi PCMSK,0 // for pcint0

    // /256 prescaler
    in tmp1,TCCR0B
    ori tmp1,lo8(4)
    out TCCR0B,tmp1

    ldi tmp1,lo8(80) // about 2.7ms
    out OCR0A,tmp1

    // Enable compare interrupt (a)
    in tmp1,TIMSK
    ori tmp1,lo8(16)
    out TIMSK,tmp1

    out TCNT0,__zero_reg__
    sei

    // Setup sleep mode
    in tmp1,MCUCR
    ori tmp1,lo8(24)
    out MCUCR,tmp1

mainLoop:
    sleep
    rjmp mainLoop
