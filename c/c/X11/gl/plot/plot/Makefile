VERSION_MAJOR=1
VERSION_MINOR=2
VERSION_PATCH=8
VERSION=$(VERSION_MAJOR).$(VERSION_MINOR).$(VERSION_PATCH)
DEFINES=-DDEBUG -DVERSION=\"$(VERSION)\"
X_DEFINES=-DX11COMPILE
V_DEFINES=-DVGACOMPILE
INCLUDES=-I/usr/X11R6/include
CFLAGS=-O2 $(DEFINES) $(INCLUDES)
XLIBS=-L/usr/X11R6/lib -lX11
VGALIBS=-lvga
LDFLAGS=-s
X_OBJS=xplot.o X11.o
V_OBJS=vgaplot.o vga.o
C_OBJS=convert.o
OBJS=$(X_OBJS) $(V_OBJS) $(C_OBJS)
SOURCES=plot.c X11.c vga.c convert.c unconvert.c
OTHER=plot.h data.h xplot_icon.xbm coast.dat README
EXES=xplot vgaplot convert
NAME=plot
JUNK=$(OBJS) $(EXES) coast.bin *~

all: xplot vgaplot convert tags

convert: $(C_OBJS)
	cc $(CFLAGS) -o convert $(C_OBJS) $(LDFLAGS)

convert: unconvert.o
	cc $(CFLAGS) -o unconvert unconvert.o $(LDFLAGS)

xplot: $(X_OBJS)
	cc $(CFLAGS) $(X_DEFINES) -o xplot $(X_OBJS) $(LDFLAGS) $(XLIBS)

vgaplot: $(V_OBJS)
	cc $(CFLAGS) $(V_DEFINES) -o vgaplot $(V_OBJS) $(LDFLAGS) $(VGALIBS)

xplot.o: plot.c
	cc $(CFLAGS) $(X_DEFINES) -c -o xplot.o plot.c

vgaplot.o: plot.c
	cc $(CFLAGS) $(V_DEFINES) -c -o vgaplot.o plot.c

tags: $(SOURCES)
	ctags *.c *.h

dist: $(SOURCES)
	@mkdir $(NAME)-$(VERSION)
	@cp Makefile $(SOURCES) $(OTHER) $(NAME)-$(VERSION)
	@tar -czvf $(NAME)-$(VERSION).tgz $(NAME)-$(VERSION)
	@rm -rf $(NAME)-$(VERSION)

clean:
	@echo Cleaning up...
	@rm -f $(JUNK)
