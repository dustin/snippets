#include <stdio.h>
#include <stdlib.h>
#include <dSNPP.h>

void main(int argc, char **argv)
{
    id snpp;
    snpp=[[dSNPP alloc] init];
    [snpp connectTo :"pager" :1041];
    [snpp sendAPage :"dustin" thatsays:"Hey, what's up? (from objc snpp)"];
    [snpp reset];
    [snpp sendAPage :"dustin" thatsays:"Hey, what's up? (high, from objc snpp)"
          priority:"high"];
    [snpp quit];
}
