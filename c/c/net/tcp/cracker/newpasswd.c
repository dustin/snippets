/*
 * This stuff was all written by James Lemley then stolen by me.
 *
 * $Id: newpasswd.c,v 1.4 2000/02/24 00:48:47 dustin Exp $
 */

#include "cracker.h"

/*
 * char keychars[] = "  abcdefghijklmnopqrstuvwxyz"
 * "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 * "0123456789"
 * "{}[]~!@#$%^&*()_+`-=:;\"'|\\<,>.?/";
 */

#define keychars " \tabcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" \
"0123456789{}[]~!@#$%^&*()_+`-=:;\"'|\\<,>.?/"

char   *
newpasswd(char *in, int *passmap)
{

	/* all characters that might be used in a password */
	register int i;

	/* update the passmap */
	i = 0;
	while (i < 8) {
		if (passmap[i] == sizeof(keychars) - 2) {
			passmap[i] = 1;
			in[i] = keychars[1];
			i++;
		} else {
			passmap[i]++;
			in[i] = keychars[passmap[i]];
			break;
		}
	}
	return (in);
}

void
set_pass(char *p, char *in, int *passmap)
{
	int     i, j;
	int     len;

	for(i=0; i<8; i++) {
		passmap[i]=0;
		in[i]=0;
	}
	in[i]=0;

	len = strlen(p);
	if (len > 8)
		len = 8;

	for (i = 0; i < len; i++) {
		for (j = 0; j < sizeof(keychars); j++)
			if (p[i] == keychars[j])
				break;
		passmap[i] = j;
		if (j == sizeof(keychars)) {
			printf("Can't restart with this password\n");
			exit(1);
		}
	}
	strcpy(in, p);
}
