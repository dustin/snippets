/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: find.c,v 1.3 1997/07/15 13:50:27 dustin Exp $
 */

#include <stdio.h>
#include <unistd.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <Xm/TextF.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>

#include "phonebook.h"
#include "fields.h"

extern progdata globl;
extern char **dbrnames, **dbfnames;

static char *relentries[]={
    "is",
    "is not",
    "is like",
    "is not like",
    NULL
};

static char *relquery[]={
    " = ",
    " != ",
    " like ",
    " not like "
};

SearchCB cbdata;

void Find(Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget frm, pane, rc, b, list, opt;
    XmString item;
    int n, i;
    Arg args[MAX_ARGS];

    n=0;
    XtSetArg(args[n], XmNallowShellResize, True); n++;
    cbdata.top=XmCreateDialogShell(globl.parent, "findbox", args, n);
    XtManageChild(cbdata.top);

    n=0;
    /* XtSetArg(args[n], XmNpacking, XmPACK_COLUMN); n++; */
    frm=XmCreateRowColumn(cbdata.top, "container", args, n);
    XtManageChild(frm);

    n=0;
    XtSetArg(args[n], XmNnumColumns, 2); n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    rc=XmCreateRowColumn(frm, "inputs", args, n);
    XtManageChild(rc);

    n=0;
    XtManageChild(XmCreateLabelGadget(rc, "Find entries where", args, n));

    n=0;
    opt=XmCreatePulldownMenu(rc, "pane", args, n);

    for(i=0; i<NFIELDS; i++)
    {
	n=0;
	b=XmCreatePushButtonGadget(opt, dbrnames[i], args, n);
        XtAddCallback(b, XmNactivateCallback, FindSetFrom, (XtPointer) i);
	XtManageChild(b);
    }

    n=0;
    XtSetArg(args[n], XmNsubMenuId, opt); n++;
    list=XmCreateOptionMenu(rc, "fromMenu", args, n);
    XtManageChild(list);

    /* relation */

    n=0;
    opt=XmCreatePulldownMenu(rc, "pane", args, n);

    for(i=0; relentries[i]!=NULL; i++)
    {
	n=0;
	b=XmCreatePushButtonGadget(opt, relentries[i], args, n);
        XtAddCallback(b, XmNactivateCallback, FindSetRelation, (XtPointer) i);
	XtManageChild(b);
    }

    n=0;
    XtSetArg(args[n], XmNsubMenuId, opt); n++;
    list=XmCreateOptionMenu(rc, "relationMenu", args, n);
    XtManageChild(list);

    /* end of relation */

    n=0;
    cbdata.value=XmCreateTextField(rc, "findsel", args, n);
    XtManageChild(cbdata.value);

    n=0;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    XtSetArg(args[n], XmNnumColumns, 2); n++;
    rc=XmCreateRowColumn(frm, "inputs", args, n);
    XtManageChild(rc);

    n=0;
    b=XmCreatePushButtonGadget(rc, "Go", args, n);
    XtAddCallback(b, XmNactivateCallback, FindCB, (XtPointer) &cbdata);
    XtManageChild(b);

    n=0;
    b=XmCreatePushButtonGadget(rc, "Close", args, n);
    XtAddCallback(b, XmNactivateCallback, CloseFindWindow, NULL);
    XtManageChild(b);
}

void FindSetFrom(Widget w, XtPointer client_data, XtPointer call_data)
{
    cbdata.field=(int)client_data;
}

void FindSetRelation(Widget w, XtPointer client_data, XtPointer call_data)
{
    cbdata.relation=(int)client_data;
}

void FindCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    XmString data;
    char query[120];
    Widget data2;
    Arg args[MAX_ARGS];
    int n;

    strcpy(query, "select * from phone where ");
    strcat(query, dbfnames[cbdata.field]);
    strcat(query, relquery[cbdata.relation]);
    strcat(query, "'");

    data=XmTextFieldGetString(cbdata.value);

    strcat(query, data);
    strcat(query, "'");

    CreateTrans(query, "your query");
    doQuery(query);
}

void CloseFindWindow(Widget w, XtPointer client_data, XtPointer call_data)
{
    XtUnmanageChild(cbdata.top);
}
