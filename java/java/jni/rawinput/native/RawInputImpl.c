/*
 * Copyright (c) 2000 Dustin Sallings <dustin@spy.net>
 *
 * $Id: RawInputImpl.c,v 1.1 2000/10/18 09:53:14 dustin Exp $
 */

#include <jni.h>
#include "net_spy_jni_RawInput.h"
#include <stdio.h>
#include <termios.h>

JNIEXPORT void JNICALL
Java_net_spy_jni_RawInput_setRawInput(JNIEnv *env, jobject obj)
{
	struct termios term;

	/* Stdin only */
	tcgetattr(0, &term);
	term.c_lflag&=~(ISIG|ICANON|ECHO);
	tcsetattr(0, TCSANOW, &term);
	return;
}

JNIEXPORT void JNICALL
Java_net_spy_jni_RawInput_unsetRawInput(JNIEnv *env, jobject obj)
{
	struct termios term;

	/* Stdin only */
	tcgetattr(0, &term);
	term.c_lflag|=(ISIG|ICANON|ECHO);
	tcsetattr(0, TCSANOW, &term);
	return;
}
