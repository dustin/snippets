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
__one_reg__ = 2
pinb = 16
prev = 17
current = 18
out = 19

    .text


.global __vector_10
    .type   __vector_10, @function

// Timer ISR
__vector_10:
    ldi current,lo8(1)
    reti

    .size   __vector_10, .-__vector_10

.section    .text.startup,"ax",@progbits
.global main
.type   main, @function

main:
    clr __zero_reg__
    clr __one_reg__
    inc __one_reg__

    ldi tmp1,lo8(0b00111110) // not the ppm pin
    out DDRB,tmp1

    cli
    // /256 prescaler
    ldi tmp1,lo8(4)
    out TCCR0B,tmp1

    ldi tmp1,lo8(80) // about 2.7ms
    out OCR0A,tmp1

    // Enable compare interrupt (a)
    ldi tmp1,lo8(16)
    out TIMSK,tmp1

    out TCNT0,__zero_reg__
    sei

    ldi current,lo8(1)
    clr prev

mainLoop:
    in pinb,PINB
    andi pinb,lo8(1)
    cp prev,pinb
    breq mainLoop

changed:
    clr out
    cpse pinb,__zero_reg__
    mov out, current

output:
    out PORTB,out

    out TCNT0,__zero_reg__
    mov prev,pinb
    cpse pinb,__one_reg__
    lsl current
    andi current,lo8(0b11110)

    rjmp mainLoop
