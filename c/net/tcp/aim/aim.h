/*
 * Copyright (c) 1999  Dustin Sallings
 *
 * $Id: aim.h,v 1.2 1999/06/13 07:02:11 dustin Exp $
 */

#ifndef _AOL_H_
#define _AOL_H_ 1

#ifndef HAVE_SNPRINTF
int snprintf(char *s, size_t n, const char *format, ...);
#endif

#ifndef HAVE_VSNPRINTF
# ifndef HAVE_VSPRINTF
#  error No vsprintf and no vsnprintf, what is this?
# else
#  define vsnprintf(a, b, c, d) vsprintf(a, c, d); assert(strlen(a)<b)
# endif
#endif

#ifndef _ndebug
# define _ndebug(a,b) if(aol->debug>=a) printf b; fflush(stdout)
#endif

#define AOL_BUF_LEN 1024
#define AOL_ROAST_STRING "Tic/Toc"
#define AOL_DEFAULT_STRLEN 256

#define CONDFREE { if(a) { free(a); a=0; } }

struct __aim;
typedef struct __aim AIM;

struct __aim {
	int debug;      /* Debug level */
	int socket;     /* Socket */
	char *hostname; /* Hostname to connect to */
	int port;       /* Port to connect to */

	char *username; /* AIM UserID */
	char *pass;     /* AIM Password */

	struct {
		int buf_begin;
		int buf_current;
		int buf_end;
		int buf_size;
		char *buffer;
	} indata;

	/* Initialization stuff */
	int (*connect)(AIM *aol);
	int (*logout)(AIM *aol);
	void (*destroy)(AIM *aol);
};

char *_aol_hexprint(int size, char *buf);
char *_aol_killwhitey(char *in);
char *_aol_kw(char *command);
char *_aol_strappend(int *size, char *dest, char *str);
char *_aol_unhexprint(int size, char *buf);
char *aol_roast(char *string);

#endif /* _AOL_H_ */
