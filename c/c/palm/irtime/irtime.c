/*
 * IrTest
 * Tom Dyas (tdyas@vger.rutgers.edu)
 *
 * This program is a test program for the IrComm library on PalmOS 3.
 */

#include <Common.h>
#include <System/SysAll.h>
#include <UI/UIAll.h>
#include <System/irlib.h>

#include "irtime.h"
#include "callback.h"

typedef struct
{
  RectangleType bounds;
  SWord X, Y;
} TtyGadget;

static Word irRefNum;
static IrConnect conn;
static TtyGadget tty;
static IrDeviceAddr deviceAddr;
static IrPacket packet;
static char buffer[128];

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

static void
PrintStatus(CharPtr where, IrStatus status)
{
  Print(where);
  Print(": ");
  switch (status) {
  case IR_STATUS_SUCCESS:
    Print("Success.");
    break;
  case IR_STATUS_FAILED:
    Print("Failed.");
    break;
  case IR_STATUS_PENDING:
    Print("Pending.");
    break;
  case IR_STATUS_DISCONNECT:
    Print("Disconnect.");
    break;
  case IR_STATUS_NO_IRLAP:
    Print("No IrLAP.");
    break;
  case IR_STATUS_MEDIA_BUSY:
    Print("Media busy.");
    break;
  case IR_STATUS_MEDIA_NOT_BUSY:
    Print("Media not busy.");
    break;
  case IR_STATUS_NO_PROGRESS:
    Print("No progress.");
    break;
  case IR_STATUS_LINK_OK:
    Print("Link OK.");
    break;
  }
  Print("\n");
}

static void
PrintCallbackEvent(CharPtr where, IrEvent eventType)
{
  Print(where);
  Print(": ");
  switch (eventType) {
  case LEVENT_LM_CON_IND:
    Print("LEVENT_LM_CON_IND");
    break;
  case LEVENT_LM_DISCON_IND:
    Print("LEVENT_LM_DISCON_IND");
    break;
  case LEVENT_DATA_IND:
    Print("LEVENT_DATA_IND");
    break;
  case LEVENT_PACKET_HANDLED:
    Print("LEVENT_PACKET_HANDLED");
    break;
  case LEVENT_LAP_CON_IND:
    Print("LEVENT_LAP_CON_IND");
    break;
  case LEVENT_LAP_DISCON_IND:
    Print("LEVENT_LAP_DISCON_IND");
    break;
  case LEVENT_DISCOVERY_CNF:
    Print("LEVENT_DISCOVERY_CNF");
    break;
  case LEVENT_LAP_CON_CNF:
    Print("LEVENT_LAP_CON_CNF");
    break;
  case LEVENT_LM_CON_CNF:
    Print("LEVENT_LM_CON_CNF");
    break;
  case LEVENT_STATUS_IND:
    Print("LEVENT_STATUS_IND");
    break;
  case LEVENT_TEST_IND:
    Print("LEVENT_TEST_IND");
    break;
  case LEVENT_TEST_CNF:
    Print("LEVENT_TEST_CNF");
    break;
  }
  Print("\n");
}

static void
OurCallback(IrConnect * conn, IrCallBackParms * params)
{
  Char buf[32];
  CALLBACK_PROLOGUE;

  PrintCallbackEvent("callback: ", params->event);
  switch (params->event) {
  case LEVENT_DISCOVERY_CNF:
    StrIToA(buf, params->deviceList->nItems);
    Print("DISCOVERY_CNF: ");
    Print(buf);
    Print(" devices discovered.\n");
    if (params->deviceList->nItems >= 1) {
      deviceAddr = params->deviceList->dev[0].hDevice;
    }
    break;
  case LEVENT_STATUS_IND:
    switch (params->status) {
    case IR_STATUS_NO_PROGRESS:
      Print("IR_STATS_NO_PROGRESS\n");
      break;
    case IR_STATUS_LINK_OK:
      Print("IR_STATUS_LINK_OK\n");
      break;
    case IR_STATUS_MEDIA_NOT_BUSY:
      Print("IR_STATUS_MEDIA_NOT_BUSY\n");
      break;
    }
  }
  CALLBACK_EPILOGUE;
}

static void
do_bind(void)
{
  IrStatus status;

  MemSet(&conn, sizeof(conn), 0);

  status = IrBind(irRefNum, &conn, OurCallback);
  PrintStatus("IrBind", status);
}

static void
do_unbind(void)
{
  IrStatus status;

  status = IrUnbind(irRefNum, &conn);
  PrintStatus("IrUnbind", status);
}

static void
do_discover(void)
{
  IrStatus status;

  status = IrDiscoverReq(irRefNum, &conn);
  PrintStatus("IrDiscoverReq", status);
}

static void
do_connect_lap()
{
  IrStatus status;

  status = IrConnectIrLap(irRefNum, deviceAddr);
  PrintStatus("IrConnectIrLap", status);
}

static void
do_disconnect_lap()
{
  IrStatus status;

  status = IrDisconnectIrLap(irRefNum);
  PrintStatus("IrDisconnectIrLap", status);
}

static void
do_test()
{
  IrStatus status;

  MemSet(&buffer, 0, sizeof(buffer));
  packet.buff = &buffer[0];
  packet.len = 8;

  status = IrTestReq(irRefNum, deviceAddr, &conn, &packet);
  PrintStatus("IrTestReq", status);
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
    case ctlID_TestButton:
      do_test();
      return true;
    case ctlID_DiscoverButton:
      do_discover();
      return true;
    case ctlID_BindButton:
      do_bind();
      return true;
    case ctlID_UnbindButton:
      do_unbind();
      return true;
    case ctlID_ConnectLAPButton:
      do_connect_lap();
      return true;
    case ctlID_DisconnectLAPButton:
      do_disconnect_lap();
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
  IrStatus status;

  deviceAddr.u32 = 0;

  /* Initialize the tty gadget. */
  RctSetRectangle(&tty.bounds, 0, 70, 160, 8 * FntCharHeight());
  WinEraseRectangle(&tty.bounds, 0);
  tty.X = tty.Y = 0;

  /* Locate the IR shared library. (should have been autoloaded already) */
  err = SysLibFind(irLibName, &irRefNum);
  if (err != 0) {
    FrmCustomAlert(alertID_Error, "StartApplication: SysFindLib failed",
		   " ", " ");
    return 1;
  }

  /* Let the IR library allocate space and other stuff. */
  err = IrOpen(irRefNum, 0);
  if (err != 0) {
    FrmCustomAlert(alertID_Error, "StartApplication: IrOpen failed",
		   " ", " ");
    return 1;
  }

  FrmGotoForm(formID_MainForm);
  return 0;
}

static void
StopApplication(void)
{
  FrmCloseAllForms();

  /* Let the IR library free any resources. (must balance IrOpen() )*/
  IrClose(irRefNum);
}

DWord
PilotMain (Word cmd, Ptr cmdPBP, Word launchFlags)
{
  if (cmd == sysAppLaunchCmdNormalLaunch)
    {
      if (! StartApplication()) {
	EventLoop();
	StopApplication();
      }
    }
  return 0;
}
