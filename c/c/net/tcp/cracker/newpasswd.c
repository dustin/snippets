/*
 * This stuff was all written by James Lemley then stolen by me.
 *
 * $Id: newpasswd.c,v 1.3 1997/01/10 05:58:44 dustin Exp $
 */

#include "cracker.h"

extern struct global *glob;

/*
char keychars[] = "  abcdefghijklmnopqrstuvwxyz"
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                  "0123456789"
                  "{}[]~!@#$%^&*()_+`-=:;\"'|\\<,>.?/";
*/

#define keychars "  abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\
0123456789{}[]~!@#$%^&*()_+`-=:;\"'|\\<,>.?/"

char *newpasswd()
{

    /* all characters that might be used in a password */
    register int i;

        /* update the passmap */
        i = 0;
        while (i < 8)
        {
                if (glob->passmap[i] == sizeof(keychars)-2)
                {
                        glob->passmap[i] = 1;
                        glob->password[i] = keychars[1];
                        i++;
                }
                else
                {
                        glob->passmap[i]++;
                        glob->password[i] = keychars[glob->passmap[i]];
                        break;
                }
        }
        return (glob->password);
}

void set_pass(char *p)
{
        int i, j;
        int len;

        len = strlen(p);
        if (len > 8)
                len = 8;

        for (i = 0; i < len; i++)
        {
                for (j = 0; j < sizeof(keychars); j++)
                        if (p[i] == keychars[j])
                                break;
                glob->passmap[i] = j;
                if (j == sizeof(keychars))
                {
                        printf("Can't restart with this password\n");
                        exit(1);
                }
        }
        strcpy(glob->password, p);
}
