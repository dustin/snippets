dnl Copyright (c) 2002  Dustin Sallings <dustin@spy.net>
dnl
dnl $id$

dnl Let's see if the threads actually work
AC_DEFUN(SPY_PTHREAD, [
AC_CACHE_CHECK([for working pthread], auth_cv_working_pthread, [
AC_TRY_RUN([
/* Need this for Solaris */
#include <stdio.h>
#include <stdlib.h>
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif /* HAVE_SYS_TYPES_H */
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */

void *thread_main(void *arg) {
	exit(0);
}

int main(int argc, char **argv) {
	pthread_t t;
	/* If the thread actually executes, the program will exit 0 */
	pthread_create(&t, NULL, thread_main, NULL);
	sleep(2);
	exit(1);
}
], [
		dnl Test worked
		auth_cv_working_pthread=yes
	],
	[
		dnl Test failed
		auth_cv_working_pthread=no
	],
	[
		dnl Cross-compiling
		auth_cv_working_pthread=no
	]
	)
])

if test "x$auth_cv_working_pthread" = "xyes"
then
	AC_DEFINE_UNQUOTED(WORKING_PTHREAD, 1)
	AC_SUBST(WORKING_PTHREAD)
fi])

