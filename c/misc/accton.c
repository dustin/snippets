#include <unistd.h>

int main(int argc, char **argv)
{
	int rv=0;
	if(argc>0) {
		rv=acct(argv[1]);
	} else {
		rv=acct(0);
	}

	if(rv<0) {
		perror("acct");
	}
}
