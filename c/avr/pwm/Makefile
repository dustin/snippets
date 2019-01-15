MCU=atmega328p
CC=avr-gcc
OBJCOPY=avr-objcopy
CFLAGS=--std=c99 -Wall -Os -DF_CPU=16000000L -mmcu=$(MCU)
LDFLAGS=-mmcu=$(MCU)

pwm.hex: pwm.elf
	avr-objcopy -j .text -j .data -O ihex pwm.elf pwm.hex
	avr-size --format=avr --mcu=$(MCU) pwm.elf

pwm.elf: pwm.o
	${CC} $(LDFLAGS) -o pwm.elf pwm.o

pwm.S: pwm.c
	avr-gcc -Wa,-gstabs $(CFLAGS) -S -o pwm.S pwm.c

clean:
	rm pwm.{elf,hex,o,S}

install: pwm.hex
	# avrdude -c usbtiny -p t85 -U flash:w:pwm.hex:i
	avrdude -v -p $(MCU) -c arduino -P /dev/tty.SLAB_USBtoUART -b 57600 -D -U flash:w:pwm.hex:i
