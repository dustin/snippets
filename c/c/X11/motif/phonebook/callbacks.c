/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: callbacks.c,v 1.4 1997/10/01 07:07:20 dustin Exp $
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

void UnImplemented(Widget w, XtPointer client_data, XtPointer call_data)
{
    CreateTrans("Sorry, but that feature isn't implemented yet.",
        "unimplemented");
}

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
    char query[8192];

    strcpy(query, "insert into phone (");

    for(i=0; i<NFIELDS; i++)
    {
	strcat(query, dbfnames[i]);
	strcat(query, ", ");
    }

    strcat(query, ") values(");

    for(i=0; i<NFIELDS-1; i++)
    {
	data=XmTextFieldGetString(info->data[i]);
	strcat(query, "'");
	strcat(query, data);
	strcat(query, "', ");
    }
    data=XmTextFieldGetString(info->data[i]);
    strcat(query, "'");
    strcat(query, data);
    strcat(query, "');");

    puts(query);

    CreateTrans(query, "Query");
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
