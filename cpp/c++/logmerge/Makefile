CPPFLAGS=-g
LDFLAGS=-g
CXX=c++

OBJS=LogFile.o

test: $(OBJS) test.o
	$(CXX) $(LDFLAGS) -o $@ test.o $(OBJS)
