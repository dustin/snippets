DEFINES=-DUSE_SSLEAY
INCLUDES=-I/usr/local/ssl/include
LIBS=-L/usr/local/ssl/lib -lssl -lcrypto
CFLAGS=$(DEFINES) $(INCLUDES) -O2
LDFLAGS=$(LIBS)

httppost: http.o post.o
	$(CC) -o $@ http.o post.o $(LDFLAGS)
