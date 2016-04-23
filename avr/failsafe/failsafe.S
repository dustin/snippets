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
prevb = 17
overflows = 18
__tmp_reg__ = 0
__zero_reg__ = 1
	.text

// Pin change ISR
.global	__vector_2
	.type	__vector_2, @function
__vector_2:
	wdr

	in r25,PINB
    mov r24,prevb
    mov prevb,r25
	eor r25,r24

	sbrc r24,0  // Skip next instruction if signal has changed
	cbi PORTB,1 // signal changed

	sbrs r24,2 // Skip next instruction if button is not changed
	rjmp pinChangeDone

buttonChanged:
	sbis PINB,2  // Skip next instruction if button is up
	rjmp buttonDown

buttonUp:
	in r24,TIMSK
	andi r24,lo8(-3)
	out TIMSK,r24
	rjmp pinChangeDone

buttonDown:
	in r24,TIMSK
	ori r24,lo8(2)
	out TIMSK,r24
	clr overflows

pinChangeDone:
	reti
	.size	__vector_2, .-__vector_2

// Watchdog ISR
.global	__vector_12
	.type	__vector_12, @function
__vector_12:
	wdr
	sbi PORTB,1
	reti
	.size	__vector_12, .-__vector_12

// Timer overflow ISR.
.global	__vector_5
	.type	__vector_5, @function
__vector_5:
	clr __zero_reg__
	subi overflows,lo8(-(1))
	cpi overflows,lo8(30)
	brlo timerDone

    // Tear it down
	out PCMSK,__zero_reg__
	sbi PORTB,3
	out MCUSR,__zero_reg__
	in r24,WDTCR
	ori r24,lo8(24)
	out WDTCR,r24
	out WDTCR,__zero_reg__
	out TIMSK,__zero_reg__

timerDone:
	reti
	.size	__vector_5, .-__vector_5
	.section	.text.startup,"ax",@progbits


// Main
.global	main
	.type	main, @function
main:
	ldi r24,lo8(10)
	out DDRB,r24
	sbi PORTB,2

	cli
    // Pin changes for input and button
	in r24,GIMSK
	ori r24,lo8(32)
	out GIMSK,r24
	in r24,PCMSK
	ori r24,lo8(5)
	out PCMSK,r24

    // Prescaler at clk/1024
	in r24,TCCR0B
	ori r24,lo8(5)
	out TCCR0B,r24

    // And watchdog interrupts at ~1S
	wdr
	ldi r24,lo8(94)
	out WDTCR,r24
	sei

    // Setup sleep mode
	in r24,MCUCR
	ori r24,lo8(24)
    out MCUCR,r24

mainLoop:
    sleep ; Put MCU in sleep mode
	rjmp mainLoop

.size	main, .-main


.global __do_copy_data
.global __do_clear_bss
