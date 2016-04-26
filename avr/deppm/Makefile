CC=avr-gcc
OBJCOPY=avr-objcopy
ASFLAGS=-mmcu=attiny85
LDFLAGS=-mmcu=attiny85

deppm.hex: deppm.elf
	avr-objcopy -j .text -j .data -O ihex deppm.elf deppm.hex
	avr-size --format=avr --mcu=attiny85 deppm.elf

deppm.elf: deppm.o
	${CC} $(LDFLAGS) -o deppm.elf deppm.o

deppm.o: deppm.S
	avr-gcc $(ASFLAGS) -c -o deppm.o deppm.S

clean:
	rm deppm.{elf,hex,o}

install: deppm.hex
	avrdude -c usbtiny -p t85 -U flash:w:deppm.hex:i
