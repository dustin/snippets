/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: find.c,v 1.1 1997/06/16 13:50:42 dustin Exp $
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

void Find(Widget w, XtPointer client_data, XtPointer call_data)
{
    Widget frm, pane, rc, b, list;
    XmString item;
    int n, i;
    Arg args[MAX_ARGS];
    SearchCB *cbdata;

    cbdata=(SearchCB *)malloc(sizeof(SearchCB));

    n=0;
    XtSetArg(args[n], XmNallowShellResize, True); n++;
    cbdata->top=XmCreateDialogShell(globl.parent, "findbox", args, n);
    XtManageChild(cbdata->top);

    n=0;
    /* XtSetArg(args[n], XmNpacking, XmPACK_COLUMN); n++; */
    frm=XmCreateRowColumn(cbdata->top, "container", args, n);
    XtManageChild(frm);

    n=0;
    XtSetArg(args[n], XmNnumColumns, 2); n++;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    rc=XmCreateRowColumn(frm, "inputs", args, n);
    XtManageChild(rc);

    n=0;
    XtManageChild(XmCreateLabelGadget(rc, "Find entries where", args, n));

    n=0;
    cbdata->field=XmCreatePulldownMenu(rc, "pane", args, n);

    for(i=0; i<NFIELDS; i++)
    {
	int n=0;
	cbdata->buttons[i]=XmCreatePushButtonGadget(cbdata->field, dbrnames[i], args, n);
	XtManageChild(cbdata->buttons[i]);
    }

    n=0;
    XtSetArg(args[n], XmNsubMenuId, cbdata->field); n++;
    list=XmCreateOptionMenu(rc, "thing", args, n);
    XtManageChild(list);

    n=0;
    XtManageChild(XmCreateLabelGadget(rc, "is like", args, n));

    n=0;
    cbdata->value=XmCreateTextField(rc, "findsel", args, n);
    XtManageChild(cbdata->value);

    n=0;
    XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
    XtSetArg(args[n], XmNnumColumns, 2); n++;
    rc=XmCreateRowColumn(frm, "inputs", args, n);
    XtManageChild(rc);

    n=0;
    b=XmCreatePushButtonGadget(rc, "Go", args, n);
    XtAddCallback(b, XmNactivateCallback, FindCB, (XtPointer) cbdata);
    XtManageChild(b);

    n=0;
    b=XmCreatePushButtonGadget(rc, "Cancel", args, n);
    XtAddCallback(b, XmNactivateCallback, CloseFindWindow, (XtPointer) cbdata);
    XtManageChild(b);
}

void FindCB(Widget w, XtPointer client_data, XtPointer call_data)
{
    SearchCB *sdb=(SearchCB *) client_data;
    XmString data;
    Widget data2;
    Arg args[MAX_ARGS];
    int n;

    n=0;
    XtSetArg(args[n], XmNmenuHistory, &data2);
    XtGetValues(sdb->field, args, n);

    /* puts(data2); */

    n=0;
    XtSetArg(args[n], XmNlabelString, &data);
    XtGetValues(data2, args, n);

    puts(data);

    data=XmTextFieldGetString(sdb->value);
    puts(data);
}

void CloseFindWindow(Widget w, XtPointer client_data, XtPointer call_data)
{
    SearchCB *sdb=(SearchCB *) client_data;

    XtUnmanageChild(sdb->top);
    free(sdb);
}
