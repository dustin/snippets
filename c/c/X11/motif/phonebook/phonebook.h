/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: phonebook.h,v 1.2 1997/06/16 13:50:43 dustin Exp $
 */

#ifndef PHONEBOOK_H
#define PHONEBOOK_H 1

#define MAX_ARGS 5

#include "fields.h"

typedef struct {
    Widget data[NFIELDS];
} infotype;

typedef struct {
    Widget parent;
} progdata;

typedef struct {
    Widget field;
    Widget top;
    Widget relation;
    Widget value;
    Widget buttons[NFIELDS];
} SearchCB;

#define TRANSBUTTON "Sure"

#define ABOUT_TEXT "\
Dustin's Phonebook program\n\
One day, this will be useful.\
"

#define HELP_TEXT "\
I help others who help themselves.  (RTFM)"

void Quit(Widget w, XtPointer client_data, XtPointer call_data);
void Help(Widget w, XtPointer client_data, XtPointer call_data);
void About(Widget w, XtPointer client_data, XtPointer call_data);
void ShowFields(Widget w, XtPointer client_data, XtPointer call_data);
void Store(Widget w, XtPointer client_data, XtPointer call_data);
void Find(Widget w, XtPointer client_data, XtPointer call_data);
void FindCB(Widget w, XtPointer client_data, XtPointer call_data);
void CloseFindWindow(Widget w, XtPointer client_data, XtPointer call_data);

void initfields(void);
void freefields(void);

#endif /* PHONEBOOK_H */
