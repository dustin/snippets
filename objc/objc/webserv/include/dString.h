#include <objc/Object.h>

@interface dString : Object
{
@private
    char *string;   /* pointer to the string itself */
    int size;       /* Amount of allocated memory   */
    int inc;        /* Amount to increment on grow  */
}
-init;
-init:(int)s;
-appendString:(const char *)astring;
- (int) length;
-lowercase;
-uppercase;
-clear;
-print;
@end
