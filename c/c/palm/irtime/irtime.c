/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: irtime.c,v 1.2 2000/08/22 07:36:12 dustin Exp $
 */

#include <Common.h>
#include <System/SysAll.h>
#include <System/ExgMgr.h>
#include <System/ErrorMgr.h>
#include <System/StringMgr.h>
#include <UI/UIAll.h>

#include "irtime.h"
#include "callback.h"

#define myAppId 'IRTM'
#define TimeType "application/x-irtm-seconds"

typedef struct
{
  RectangleType bounds;
  SWord X, Y;
} TtyGadget;

static TtyGadget tty;

static void
Print(CharPtr msg)
{
  RectangleType vacated;

  while (*msg) {
    switch (*msg) {
    case '\n':
      if (tty.Y >= tty.bounds.extent.y - FntCharHeight()) {
	WinScrollRectangle(&tty.bounds, up, FntCharHeight(), &vacated);
	WinEraseRectangle(&vacated, 0);
	tty.X = 0; tty.Y -= FntCharHeight();
      } else {
	tty.X = 0; tty.Y += FntCharHeight();
      }
      break;
    default:
      WinDrawChars(msg, 1, tty.bounds.topLeft.x + tty.X,
		   tty.bounds.topLeft.y + tty.Y);
      tty.X += FntCharWidth(*msg);
      break;
    }
    msg++;
  }
}

#define exgErrThing(a) case a: msg=#a "\n"; break;
char *getExgError(Err e)
{
	char *msg=NULL;
	static char buf[40];
	switch(e) {
		case 0:
			break;
		exgErrThing(exgMemError)
		exgErrThing(exgErrStackInit)
		exgErrThing(exgErrUserCancel)
		exgErrThing(exgErrNoReceiver)
		exgErrThing(exgErrNoKnownTarget)
		exgErrThing(exgErrTargetMissing)
		exgErrThing(exgErrNotAllowed)
		exgErrThing(exgErrBadData)
		exgErrThing(exgErrAppError)
		exgErrThing(exgErrUnknown)
		exgErrThing(exgErrDeviceFull)
		exgErrThing(exgErrDisconnected)
		exgErrThing(exgErrNotFound)
		exgErrThing(exgErrBadParam)
		exgErrThing(exgErrNotSupported)
		exgErrThing(exgErrDeviceBusy)
		exgErrThing(exgErrBadLibrary)
		default:
			StrPrintF(buf, "Error #%d\n", e);
			msg=buf;
			break;
	}

	return(msg);
}

void displayExgError(Err e)
{
	char *msg=NULL;

	msg=getExgError(e);
	if(msg==NULL) {
		Print("no error\n");
	} else {
		Print(msg);
	}
}


void do_send()
{
	ExgSocketType exgsocket;
	ULong t=0;
	Err err;

	MemSet(&exgsocket, sizeof(exgsocket), 0x00);

	t=TimGetSeconds();

	exgsocket.description="The time";
	/* exgsocket.type=TimeType; */
	exgsocket.count=1;
	exgsocket.length=sizeof(t);
	exgsocket.target=myAppId;

	Print("Sending...\n");

	err=ExgPut(&exgsocket);
	displayExgError(err);
	if(err==0) {
		ExgSend(&exgsocket, &t, sizeof(t), &err);
		displayExgError(err);
		err=ExgDisconnect(&exgsocket, err);
		displayExgError(err);
	}

	Print("Done\n");
}

void do_enable()
{
	Err e;
	Print("Enabling...\n");
	e=ExgRegisterData(myAppId, exgRegTypeID, TimeType);
	switch(e) {
		case 0: Print("Success!\n"); break;
		default:
			displayExgError(e);
			break;
	}
}

void do_disable()
{
	Err e;
	Print("Disabling...\n");
	e=ExgRegisterData(myAppId, exgRegTypeID, NULL);
	switch(e) {
		case 0: Print("Success!\n"); break;
		default:
			displayExgError(e);
			break;
	}
}

static Boolean
MainFormHandleEvent(EventPtr event)
{
  FormPtr form;

  switch (event->eType) {
  case frmOpenEvent:
    form = FrmGetActiveForm();
    FrmDrawForm(form);
    return true;

  case ctlSelectEvent:
    switch (event->data.ctlEnter.controlID) {
    case ctlID_SendButton:
      do_send();
      return true;
    case ctlID_EnableButton:
      do_enable();
      return true;
    case ctlID_DisableButton:
      do_disable();
      return true;
    }
    break;
  }

  return false;
}

static Boolean
AppHandleEvent(EventPtr event)
{
  Word formID;
  FormPtr form;

  if (event->eType == frmLoadEvent)    {
    formID = event->data.frmLoad.formID;
    form = FrmInitForm(formID);
    FrmSetActiveForm(form);
    switch (formID) {
    case formID_MainForm:
      FrmSetEventHandler(form, (FormEventHandlerPtr) MainFormHandleEvent);
      return true;
    default:
      ErrDisplay("unhandled form ID");
      return false;
    }
  }
  return false;
}

static void
EventLoop(void)
{
  Word err;
  EventType event;

  do {
    EvtGetEvent(&event, evtWaitForever);

    if (SysHandleEvent(&event)) continue;
    if (MenuHandleEvent((void *)0, &event, &err)) continue;
    if (AppHandleEvent(&event)) continue;
    FrmDispatchEvent(&event);
  } while (event.eType != appStopEvent);
}

static Err
StartApplication(void)
{
  Err err;

  /* Initialize the tty gadget. */
  RctSetRectangle(&tty.bounds, 0, 70, 160, 8 * FntCharHeight());
  WinEraseRectangle(&tty.bounds, 0);
  tty.X = tty.Y = 0;

  FrmGotoForm(formID_MainForm);
  return 0;
}

void alertPopup(CharPtr msg)
{
	FrmCustomAlert(alertID_Error, msg, " ", " ");
}

void infoPopup(CharPtr msg)
{
	FrmCustomAlert(alertID_Info, msg, " ", " ");
}

static void
StopApplication(void)
{
  FrmCloseAllForms();
  /* Do other stuff */
}

void receiveData(ExgSocketType *exgsocket)
{
	Err err;
	ULong t=0;

	exgsocket->goToCreator=0;

	err=ExgAccept(exgsocket);
	displayExgError(err);
	if(err==0) {
		ExgReceive(exgsocket, &t, sizeof(t), &err);
		displayExgError(err);
		if(err!=0) {
			char *msg;

			msg=getExgError(err);

			if(msg==NULL) {
				alertPopup("Null Error!");
			} else {
				alertPopup(msg);
			}
		} else {
			TimSetSeconds(t);
			infoPopup("Set time!");
		}

		err=ExgDisconnect(exgsocket, 0);
		displayExgError(err);
	} else {
		alertPopup("Did not accept");
	}
}

DWord
PilotMain (Word cmd, Ptr cmdPBP, Word launchFlags)
{
  Err inErr;
  ExgAskParamType *askparam=NULL;
  ExgSocketType *exgsocket=NULL;

  switch(cmd) {
	case sysAppLaunchCmdNormalLaunch:
		if(!StartApplication()) {
			EventLoop();
			StopApplication();
		}
		break;
	case sysAppLaunchCmdExgAskUser:
		askparam=(ExgAskParamType *)cmdPBP;
		askparam->result=exgAskOk;
		break;
	case sysAppLaunchCmdExgReceiveData:
		exgsocket=(ExgSocketType *)cmdPBP;
		receiveData(exgsocket);
		break;
	default:
		break;
  }

  return 0;
}
