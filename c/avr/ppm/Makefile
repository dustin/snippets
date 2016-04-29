CC=avr-gcc
OBJCOPY=avr-objcopy
CFLAGS=--std=c99 -Wall -Os -DF_CPU=16000000L -mmcu=atmega328p
LDFLAGS=-mmcu=atmega328p

ppm.hex: ppm.elf
	avr-objcopy -j .text -j .data -O ihex ppm.elf ppm.hex
	avr-size --format=avr --mcu=attiny85 ppm.elf

ppm.elf: ppm.o
	${CC} $(LDFLAGS) -o ppm.elf ppm.o

ppm.S: ppm.c
	avr-gcc -Wa,-gstabs $(CFLAGS) -S -o ppm.S ppm.c

clean:
	rm ppm.{elf,hex,o,S}

install: ppm.hex
	# avrdude -c usbtiny -p t85 -U flash:w:ppm.hex:i
	avrdude -v -p atmega328p -c arduino -P /dev/tty.SLAB_USBtoUART -b 57600 -D -U flash:w:ppm.hex:i
