/*
 * This stuff was all written by James Lemley then stolen by me.
 */

#include "cracker.h"

extern struct global *glob;

char *newpasswd()
{

    /* all characters that might be used in a password */
    char keychars[] = "  abcdefghijklmnopqrstuvwxyz"
                      "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                      "0123456789"
                      "{}[]~!@#$%^&*()_+`-=:;\"'|\\<,>.?/";
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
