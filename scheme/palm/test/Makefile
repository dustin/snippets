SHELL=sh
APPID     = fbU0
APPTYPE   = Rsrc
ICONTEXT  = "TestTaker"

test.prc: test.rcp
	pilrc test.rcp .
	# Create ICON IDs 9000 and 9001 instead of 1000 ann 1001  
	-mv tAIB03e9.bin tAIB2329.bin
	-mv tAIB03e8.bin tAIB2328.bin
	/emul/linux/usr/local/bin/build-prc -t $(APPTYPE) test.prc $(ICONTEXT) $(APPID) *.bin

all:    test.prc

clean:
	rm -f *.bin test.prc
