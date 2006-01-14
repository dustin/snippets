/*
 * Copyright (c) 2005  Dustin Sallings <dustin@spy.net>
 */

enum returnvalues { ERR_DNS=-3, ERR_TIMEOUT=-2, ERR_ERRNO=-1, RV_SUCCESS=0 };

int attemptConnection(char *, char *);
