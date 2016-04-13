CC=avr-gcc
OBJCOPY=avr-objcopy
MICRONUCLEUS=/Users/dsal/Library/Arduino15/packages/digistump/tools/micronucleus/2.0a4/micronucleus
CFLAGS=-Wall -Os -DF_CPU=16000000L -mmcu=attiny85
LDFLAGS=-mmcu=attiny85

blink.hex: blink.elf
	avr-objcopy -j .text -j .data -O ihex blink.elf blink.hex
	avr-size --format=avr --mcu=attiny85 blink.elf

blink.elf: blink.o
	${CC} $(LDFLAGS) -o blink.elf blink.o

clean:
	rm blink.{elf,hex,o}

install: blink.hex
	$(MICRONUCLEUS) --run blink.hex
