/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: widgets.c,v 1.1 1997/06/16 13:46:05 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <Xm/MessageB.h>

#include "phonebook.h"

extern progdata globl;

void CreateTrans(char *text, char *name)
{
    XmString message, button_text, title;
    int n;
    Arg args[MAX_ARGS];
    Widget box, button;

    message = XmStringCreateLtoR(text, XmSTRING_DEFAULT_CHARSET);
    button_text = XmStringCreateLtoR(TRANSBUTTON,
        XmSTRING_DEFAULT_CHARSET);

    title = XmStringCreateLtoR(name, XmSTRING_DEFAULT_CHARSET);

    n = 0;
    XtSetArg(args[n], XmNdialogTitle, title); n++;
    XtSetArg(args[n], XmNokLabelString, button_text); n++;
    XtSetArg(args[n], XmNmessageString, message); n++;
    box = XmCreateMessageDialog(globl.parent, "helpbox", args, n);

    button = XmMessageBoxGetChild(box, XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(button);

    button = XmMessageBoxGetChild(box, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(button);

    if (title)
	XmStringFree(title);

    if (button_text)
	XmStringFree(button_text);

    if (message)
	XmStringFree(message);

    XtManageChild(box);
}
