/*
 * Copyright (c) 1997 Dustin Sallings
 *
 * $Id: phonebook.h,v 1.4 1997/07/15 13:50:29 dustin Exp $
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
    int dbh;
} progdata;

typedef struct {
    int field;
    int relation;
    Widget top;
    Widget value;
} SearchCB;

#define IS          0
#define ISNOT       1
#define ISLIKE      2
#define ISNOTLIKE   3

#define TRANSBUTTON "Sure"

#define ABOUT_TEXT "\
Dustin's Phonebook program\n\
One day, this will be useful.\
"

#define HELP_TEXT "\
I help others who help themselves.  (RTFM)"

void About(Widget w, XtPointer client_data, XtPointer call_data);
void CloseFindWindow(Widget w, XtPointer client_data, XtPointer call_data);
void CreateTrans(char *text, char *name);
void Find(Widget w, XtPointer client_data, XtPointer call_data);
void FindCB(Widget w, XtPointer client_data, XtPointer call_data);
void FindSetFrom(Widget w, XtPointer client_data, XtPointer call_data);
void FindSetRelation(Widget w, XtPointer client_data, XtPointer call_data);
void Help(Widget w, XtPointer client_data, XtPointer call_data);
void Quit(Widget w, XtPointer client_data, XtPointer call_data);
void ShowFields(Widget w, XtPointer client_data, XtPointer call_data);
void Store(Widget w, XtPointer client_data, XtPointer call_data);
void UnImplemented(Widget w, XtPointer client_data, XtPointer call_data);
void dbConnect(void);
void doQuery(char *query);

void initfields(void);
void freefields(void);

#endif /* PHONEBOOK_H */
