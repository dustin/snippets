DEFINES=#-DDEBUG
INCLUDES=-I/usr/people/dustin/lib/msql/include -I/usr/people/dustin/include
CFLAGS=-O2 $(INCLUDES) $(DEFINES)
LIBS=-lmsql
CGI-LIB=-lcgi
LDFLAGS=-L/home/dustin/lib -L/home/dustin/lib/msql/lib $(LIBS)
EXES=lister addhost edithost gethostinfo deletehost
OBJS=lister.o addhost.o edithost.o gethostinfo.o deletehost.o
JUNK=$(EXES) $(OBJS)
CGI=/usr/local/etc/httpd/cgi-bin/u/webtool.ipa.net/tools/hosts

all: lister addhost edithost gethostinfo deletehost

lister:	lister.o
	$(CC) $(CFLAGS) lister.o -o lister $(LDFLAGS)

addhost:	addhost.o
	$(CC) $(CFLAGS) addhost.o -o addhost $(LDFLAGS) $(CGI-LIB)

edithost:	edithost.o
	$(CC) $(CFLAGS) edithost.o -o edithost $(LDFLAGS) $(CGI-LIB)

gethostinfo:	gethostinfo.o
	$(CC) $(CFLAGS) gethostinfo.o -o gethostinfo $(LDFLAGS) $(CGI-LIB)

deletehost:	deletehost.o
	$(CC) $(CFLAGS) deletehost.o -o deletehost $(LDFLAGS) $(CGI-LIB)

install: all
	strip $(EXES)
	mv lister $(CGI)/lister.cgi
	mv addhost $(CGI)/addhost.cgi
	mv edithost $(CGI)/edithost.cgi
	mv gethostinfo $(CGI)/gethostinfo.cgi
	mv deletehost $(CGI)/deletehost.cgi
	chmod 755 $(CGI)/*

clean:
	rm -f *.core $(JUNK)
