/*
 * Copyright (c) 2000  Dustin Sallings <dustin@spy.net>
 *
 * $Id: irtime.c,v 1.4 2000/08/22 08:29:36 dustin Exp $
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

void alertPopup(CharPtr msg)
{
	FrmCustomAlert(alertID_Error, msg, " ", " ");
}
void infoPopup(CharPtr msg)
{
	FrmCustomAlert(alertID_Info, msg, " ", " ");
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
	if(msg!=NULL) {
		alertPopup(msg);
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

	err=ExgPut(&exgsocket);
	displayExgError(err);
	if(err==0) {
		ExgSend(&exgsocket, &t, sizeof(t), &err);
		displayExgError(err);
		err=ExgDisconnect(&exgsocket, err);
		displayExgError(err);
	}
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
			/* It takes about five seconds for all the data stuff to finish */
			t+=5;
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
		do_send();
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
