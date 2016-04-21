CC=avr-gcc
OBJCOPY=avr-objcopy
MICRONUCLEUS=/Users/dsal/Library/Arduino15/packages/digistump/tools/micronucleus/2.0a4/micronucleus
CFLAGS=--std=c99 -Wall -Os -DF_CPU=16000000L -mmcu=attiny85
LDFLAGS=-mmcu=attiny85

failsafe.hex: failsafe.elf
	avr-objcopy -j .text -j .data -O ihex failsafe.elf failsafe.hex
	avr-size --format=avr --mcu=attiny85 failsafe.elf

failsafe.elf: failsafe.o
	${CC} $(LDFLAGS) -o failsafe.elf failsafe.o

clean:
	rm failsafe.{elf,hex,o}

install: failsafe.hex
	avrdude -c usbtiny -p t85 -U flash:w:failsafe.hex:i
