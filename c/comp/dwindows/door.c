#include <stdlib.h>
#include <iostream.h>

#include "door.h"

char wall[MAX_X][MAX_Y];

void initwall(void)
{
int i, j;

	cout << "Initializing wall...\n";
	for(j=0; j<MAX_Y; j++)
		for(i=0; i<MAX_X; i++)
			wall[i][j]='0';

	cout << "Done.\n";
}

void displaywall(void)
{
int i, j;
	for(j=0; j<MAX_Y; j++)
		for(i=0; i<MAX_X; i++)
			cout << wall[i][j];
}

int door::opendoor(int x, int y, int width, int height, int fill)
{
	return 1;
}

int main(void)
{
	initwall();
	displaywall();
	return(0);
}
