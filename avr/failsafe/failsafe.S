__SREG__ = 0x3f
PINB = 0x16
DDRB = 0x17
PORTB = 0x18
GIMSK = 0x3b
TIMSK = 0x39
TCCR0B = 0x33
MCUSR = 0x34
WDTCR = 0x21
PCMSK = 0x15
MCUCR = 0x35
__zero_reg__ = 1
prevb = 17
overflows = 18
tmp1 = 24
tmp2 = 25

.text

// Pin change ISR
.global __vector_2
    .type   __vector_2, @function
__vector_2:
    wdr

    in tmp2,PINB
    mov tmp1,prevb
    mov prevb,tmp2
    eor tmp2,tmp1

    sbrc tmp1,0  // Skip next instruction if signal has changed
    cbi PORTB,1  // signal changed

    sbrs tmp1,2 // Skip next instruction if button is not changed
    rjmp pinChangeDone

buttonChanged:
    sbis PINB,2  // Skip next instruction if button is up
    rjmp buttonDown

buttonUp:
    in tmp1,TIMSK
    andi tmp1,lo8(-3)
    out TIMSK,tmp1
    rjmp pinChangeDone

buttonDown:
    in tmp1,TIMSK
    ori tmp1,lo8(2)
    out TIMSK,tmp1
    ldi overflows,lo8(30)

pinChangeDone:
    reti

// Watchdog ISR
.global __vector_12
    .type   __vector_12, @function
__vector_12:
    wdr
    sbi PORTB,1
    reti

// Timer overflow ISR.
.global __vector_5
    .type   __vector_5, @function
__vector_5:
    dec overflows
    brne timerDone

    // Tear it down
    clr __zero_reg__
    out PCMSK,__zero_reg__
    sbi PORTB,3
    out MCUSR,__zero_reg__
    in tmp1,WDTCR
    ori tmp1,lo8(24)
    out WDTCR,tmp1
    out WDTCR,__zero_reg__
    out TIMSK,__zero_reg__

timerDone:
    reti

.section    .text.startup,"ax",@progbits

// Main
.global main
    .type   main, @function
main:
    ldi tmp1,lo8(10)
    out DDRB,tmp1
    sbi PORTB,2

    cli
    // Pin changes for input and button
    in tmp1,GIMSK
    ori tmp1,lo8(32)
    out GIMSK,tmp1
    in tmp1,PCMSK
    ori tmp1,lo8(5)
    out PCMSK,tmp1

    // Prescaler at clk/1024
    in tmp1,TCCR0B
    ori tmp1,lo8(5)
    out TCCR0B,tmp1

    // And watchdog interrupts at ~1S
    wdr
    ldi tmp1,lo8(94)
    out WDTCR,tmp1
    sei

    // Setup sleep mode
    in tmp1,MCUCR
    ori tmp1,lo8(24)
    out MCUCR,tmp1

mainLoop:
    sleep ; Put MCU in sleep mode
    rjmp mainLoop
