#include <Xm/Xm.h> 
#include <Xm/Label.h> 
 
XtAppContext context;
XmStringCharSet char_set=XmSTRING_DEFAULT_CHARSET;
 
Widget toplevel, label;
 
main(argc,argv)
  int argc; 
  char *argv[];
{
  Arg al[10];
  int ac;
 
  /* create the toplevel shell */
  toplevel = XtAppInitialize(&context,"",NULL,0,
    &argc,argv,NULL,NULL,0);
 
  /* create label widget */
  ac=0;
  XtSetArg(al[ac],XmNlabelString, 
    XmStringCreateLtoR("Hello World", char_set)); ac++;
  label=XmCreateLabel(toplevel,"label",al,ac);
  XtManageChild(label);
 
  XtRealizeWidget(toplevel);
  XtAppMainLoop(context);
}

