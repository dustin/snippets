#include <stdio.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <Xm/Xm.h>
#include <Xm/PushB.h>

XtCallbackProc
quit_callback(Widget widget, caddr_t client_data, caddr_t call_data)
{
  exit(0);
}

int
main(int argc, char *argv[])
{
  Widget parent;
  Arg args[10];
  int n;
  Widget quit_widget;
  XtAppContext app_context;

  n = 0;
  parent = XtAppInitialize(&app_context, "Examples", (XrmOptionDescList)
			   NULL, 0, &argc, argv, (String *) NULL, args, n);

  n = 0;
  quit_widget = XtCreateManagedWidget("Quit-Program",
				  xmPushButtonWidgetClass, parent, args, n);

  XtAddCallback(quit_widget, XmNactivateCallback, quit_callback,
		(caddr_t) NULL);

  XtRealizeWidget(parent);

  XtAppMainLoop(app_context);

  exit(0);
}
