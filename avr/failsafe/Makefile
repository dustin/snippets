CC=avr-gcc
OBJCOPY=avr-objcopy
ASFLAGS=-mmcu=attiny85
LDFLAGS=-mmcu=attiny85

failsafe.hex: failsafe.elf
	avr-objcopy -j .text -j .data -O ihex failsafe.elf failsafe.hex
	avr-size --format=avr --mcu=attiny85 failsafe.elf

failsafe.elf: failsafe.o
	${CC} $(LDFLAGS) -o failsafe.elf failsafe.o

failsafe.o: failsafe.S
	avr-gcc $(ASFLAGS) -c -o failsafe.o failsafe.S

clean:
	rm failsafe.{elf,hex,o}

install: failsafe.hex
	avrdude -c usbtiny -p t85 -U flash:w:failsafe.hex:i
