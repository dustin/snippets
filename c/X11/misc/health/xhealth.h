#ifndef SIMPLEDEFINES

#define SIMPLEDEFINES
#define WIN_TITLE "Health Monitor 1.0"
#define PROGGYCLASS "NewProg"
#define PROGGYNAME "NewProg"
#define WIN_WIDTH 320
#define WIN_HEIGHT 200

#ifdef DEBUG
#define FONT "variable"
#else
#define FONT "-adobe-new century schoolbook-bold-r-normal--24-20-*-*-p-*-iso8859-1"
#endif

#define BACKFONT "variable"
#endif

#ifndef ALARMLENGTHDEFAULT
#define ALARMLENGTHDEFAULT 2*60*60
#endif

#ifndef MESSAGE
#define MESSAGE "Get up and stretch."
#endif

void init_window(void);
void keyevent(XKeyEvent * event);
void buttonevent(int x, int y, int button);
void detach(void);
void xloop();
