#include <stdio.h>
#include <assert.h>
#include <aim.h>

int main(int argc, char **argv)
{
	char *roasted;
	assert(argc>1);

	roasted=aol_roast(argv[1]);

	printf("%s roasted is %s\n", argv[1], roasted);
}
