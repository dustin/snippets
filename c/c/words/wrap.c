#include <stdio.h>

int main(int argc, char **argv)
{
	char buffer[8192];
	int i;

	while(fgets(buffer, 8192, stdin)!=NULL) {
		for(i=0; buffer[i]; i++) {
			putchar(buffer[i]);
			if(i>0 && (i%78==0)) {
				putchar('\n');
			}
		}
	}
}
