#define MAX_X 640
#define MAX_Y 480

#define VGAMODE G640x480x256

#ifdef linux
#define LINUX
#endif

void circle(int xc, int yc, int r);
void define_bounds(void);
void factor_bounds(long x, long y, float factor);

#define SX11 1
#define SVGA 0

#ifndef DEFAULTFILENAME
#define DEFAULTFILENAME "coast.dat"
#define DEFAULTBINFILENAME "coast.bin"
#endif

/*
I'm still trying to find the right font.  I like adobe-times obviously.
*/

#ifndef FONTS
#define FONTS

#define FONT "-adobe-times-medium-i-normal--0-0-100-100-p-0-iso8859-1"

/*
-adobe-times-medium-i-normal--0-0-75-75-p-0-iso8859-1
-adobe-times-medium-i-normal--10-100-75-75-p-52-iso8859-1
-adobe-times-medium-i-normal--11-80-100-100-p-52-iso8859-1
-adobe-times-medium-i-normal--12-120-75-75-p-63-iso8859-1
-adobe-times-medium-i-normal--14-100-100-100-p-73-iso8859-1
-adobe-times-medium-i-normal--14-140-75-75-p-73-iso8859-1
-adobe-times-medium-i-normal--17-120-100-100-p-84-iso8859-1
-adobe-times-medium-i-normal--18-180-75-75-p-94-iso8859-1
-adobe-times-medium-i-normal--20-140-100-100-p-94-iso8859-1
-adobe-times-medium-i-normal--24-240-75-75-p-125-iso8859-1
-adobe-times-medium-i-normal--25-180-100-100-p-125-iso8859-1
-adobe-times-medium-i-normal--8-80-75-75-p-42-iso8859-1
-adobe-times-medium-r-normal--0-0-100-100-p-0-iso8859-1
-adobe-times-medium-r-normal--10-100-75-75-p-54-iso8859-1
-adobe-times-medium-r-normal--11-80-100-100-p-54-iso8859-1
-adobe-times-medium-r-normal--12-120-75-75-p-64-iso8859-1
-adobe-times-medium-r-normal--14-140-75-75-p-74-iso8859-1
-adobe-times-medium-r-normal--17-120-100-100-p-84-iso8859-1
-adobe-times-medium-r-normal--18-180-75-75-p-94-iso8859-1
-adobe-times-medium-r-normal--20-140-100-100-p-96-iso8859-1
-adobe-times-medium-r-normal--24-240-75-75-p-124-iso8859-1
-adobe-times-medium-r-normal--25-180-100-100-p-125-iso8859-1
-adobe-times-medium-r-normal--34-240-100-100-p-170-iso8859-1
-adobe-times-medium-r-normal--8-80-75-75-p-44-iso8859-1
*/

#define BACKFONT "variable"
#endif

#ifdef X11COMPILE
void init_window(void);
void xplot(void);
void keyevent(XKeyEvent *event);
void buttonevent(int x, int y, int button);
void reportpos(int x, int y);
void busy(void);
void notbusy(void);
#endif

#ifdef VGACOMPILE
#define _setcolor(x) vga_setcolor(x)
#define _setpixel(x, y) vga_drawpixel(x, y)
void init_vga();
void vga_plot(void);
#endif
