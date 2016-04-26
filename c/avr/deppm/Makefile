CC=avr-gcc
OBJCOPY=avr-objcopy
MICRONUCLEUS=/Users/dsal/Library/Arduino15/packages/digistump/tools/micronucleus/2.0a4/micronucleus
CFLAGS=--std=c99 -Wall -Os -DF_CPU=16000000L -mmcu=attiny85
LDFLAGS=-mmcu=attiny85

deppm.hex: deppm.elf
	avr-objcopy -j .text -j .data -O ihex deppm.elf deppm.hex
	avr-size --format=avr --mcu=attiny85 deppm.elf

deppm.elf: deppm.o
	${CC} $(LDFLAGS) -o deppm.elf deppm.o

deppm.S: deppm.c
	avr-gcc -Wa,-gstabs $(CFLAGS) -S -o deppm.S deppm.c

clean:
	rm deppm.{elf,hex,o,S}

install: deppm.hex
	avrdude -c usbtiny -p t85 -U flash:w:deppm.hex:i
