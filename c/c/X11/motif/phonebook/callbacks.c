/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: callbacks.c,v 1.2 1997/06/16 13:50:41 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/List.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>

#include "phonebook.h"
#include "fields.h"

extern progdata globl;
extern char **dbrnames, **dbfnames;

void Quit(Widget w, XtPointer client_data, XtPointer call_data)
{
  freefields();
  exit(0);
}

void Help(Widget w, XtPointer client_data, XtPointer call_data)
{
    CreateTrans(HELP_TEXT, "phonebook Help");
}

void Store(Widget w, XtPointer client_data, XtPointer call_data)
{
    infotype *info=(infotype *) client_data;
    int i;
    String data;

    for(i=0; i<NFIELDS; i++)
    {
	data=XmTextFieldGetString(info->data[i]);
	printf("``%s'' -> %s\n", data, dbfnames[i]);
    }
}

void ShowFields(Widget w, XtPointer client_data, XtPointer call_data)
{
    int i;
    char str[50*NFIELDS];
    char tmp[50];

    str[0]=0x00;

    for(i=0; i<NFIELDS; i++)
    {
	sprintf(tmp, "%s -> %s\n", dbrnames[i], dbfnames[i]);
	strcat(str, tmp);
    }

    CreateTrans(str, "Field Translations");
}

void About(Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget dialog;
    XmString message, title;
    Arg args[MAX_ARGS];
    int n;

    message=XmStringCreateLtoR(ABOUT_TEXT, XmSTRING_DEFAULT_CHARSET);
    title=XmStringCreateLtoR("About Phonebook", XmSTRING_DEFAULT_CHARSET);

    n=0;
    XtSetArg(args[n], XmNmessageString, message); n++;
    XtSetArg(args[n], XmNdialogTitle, title); n++;
    dialog=XmCreateInformationDialog(globl.parent, "about", args, n);
    XtManageChild(dialog);

    XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));

    XmStringFree(message);
    XmStringFree(title);
}
