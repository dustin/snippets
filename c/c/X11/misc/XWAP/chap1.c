#include <stdio.h>
#include <X11/Xlib.h>

PrintXInfo(Display * display, int screen)
{
  int depth;

  printf("%s version %d of the %s\n", ServerVendor(display),
	 VendorRelease(display), "X Window System");

  printf("X protocol %d.%d\n", ProtocolVersion(display),
	 ProtocolRevision(display));

  depth = DefaultDepth(display, screen);

  if (depth == 1)
    printf("Color plane depth....%d (mono)\n", depth);
  else
    printf("Color plane depth....%d\n", depth);

  printf("Display Width....%d pixels\n", DisplayWidth(display,
						      screen));

  printf("Display Height....%d pixels\n", DisplayHeight(display,
							screen));

  printf("For the display [%s]\n", XDisplayName(display));
}

main(int argc, char *argv[])
{
  Display *display;

/*
 * display *ConnectToServer();
 */
  Window rootwindow;
  int screen;

  display = ConnectToServer((char *) NULL, &screen, &rootwindow);

  PrintXInfo(display, screen);

  XCloseDisplay(display);
}
