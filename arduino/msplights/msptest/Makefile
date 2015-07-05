CXXFLAGS=-Wall -Werror -I..

msptest: msp.o msptest.o
	$(CXX) -o $@ msp.o msptest.o

msp.o: ../msp.h ../msp.cpp Makefile
	$(CXX) -o $@ -c ../msp.cpp

msptest.o: msptest.cpp  Makefile
