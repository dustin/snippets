/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: geocache.c,v 1.1 2001/06/09 09:30:34 dustin Exp $
 */

#include <Common.h>
#include <System/SysAll.h>
#include <System/ExgMgr.h>
#include <System/ErrorMgr.h>
#include <UI/UIAll.h>

#include "geocache.h"
#include "callback.h"

#define myAppId 'GEOC'

static void
StopApplication(void)
{
  FrmCloseAllForms();
  /* Do other stuff */
}

static void CodeButtonPressed(void)
{
	Char *ptr=0;
	Boolean empty=false;
	FieldPtr fld;
	FormPtr frm;
	int i=0;

	frm=FrmGetFormPtr(formID_MainForm);
	fld=FrmGetObjectPtr(frm, FrmGetObjectIndex(frm, ctlID_HintInput));
	ptr=FldGetTextPtr(fld);

	/* Translate */
	for(i=0; ptr != 0x00 && ptr[i]!=0x00; i++) {
		if( (ptr[i] >= 'a' && ptr[i] <= 'm')
			|| (ptr[i] >= 'A' && ptr[i] <= 'M')) {
			ptr[i]+=13;
		} else if( (ptr[i] >= 'n' && ptr[i] <= 'z')
					|| (ptr[i] >= 'N' && ptr[i] <= 'Z')) {
			ptr[i]-=13;
		}
	}

	/* Get it to redraw */
	FrmDrawForm(FrmGetActiveForm());
}

/* Do stuff when people do stuff on the main form */
static Boolean MainFormHandler(EventType *event)
{
	FormPtr frm;
	Boolean handled=false;
	FieldPtr fldP;

	if(event->eType == ctlSelectEvent) {
		switch(event->data.ctlSelect.controlID) {
			case ctlID_CodeButton:
				CodeButtonPressed();
				handled=true;
				break;
		}
	} else if(event->eType == frmOpenEvent) {
		FrmDrawForm(FrmGetActiveForm());
		FrmSetFocus(FrmGetActiveForm(),
			FrmGetObjectIndex(FrmGetActiveForm(), ctlID_HintInput));
		handled=true;
	} else if(event->eType == fldChangedEvent) {
		handled=true;
	}

	return(handled);
}

/* When application events occur, do something */
static Boolean ApplicationHandleEvent(EventType *event)
{
	UInt16 formID=0;
	FormPtr frm;

	if(event->eType == frmLoadEvent) {
		formID = event->data.frmLoad.formID;
		frm=FrmInitForm(formID);
		FrmSetActiveForm(frm);

		switch(formID) {
			case formID_MainForm:
				FrmSetEventHandler(frm, MainFormHandler);
				break;
		}
		return(true);
	}

	return(false);
}

/* The main loop */
static void EventLoop(void)
{
	Err error=0;
	EventType event;

	do {
		EvtGetEvent(&event, evtWaitForever);

		if(!SysHandleEvent(&event)) {
			if(!MenuHandleEvent(0, &event, &error)) {
				if(!ApplicationHandleEvent(&event)) {
					FrmDispatchEvent(&event);
				}
			}
		}
	} while(event.eType!=appStopEvent);
}

/* Main */
DWord
PilotMain (Word cmd, Ptr cmdPBP, Word launchFlags)
{
  switch(cmd) {
	case sysAppLaunchCmdNormalLaunch:
		FrmGotoForm(formID_MainForm);
		EventLoop();
		StopApplication();
		break;
	default:
		break;
  }

  return 0;
}
