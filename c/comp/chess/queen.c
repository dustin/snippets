/*
Dustin Sallings
*/
#include <stdio.h>

char board[8][8];
int total;

void
initboard(void)
{
  int i, j;

  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      board[i][j] = '.';
}

void
display(void)
{
  int i, j;

  for (j = 0; j < 8; j++)
	 {
		for (i = 0; i < 8; i++)
	printf(" %c ", board[i][j]);
		putchar('\n');
	 }
}

int
horizontal(int y)
{
  int i;

  for (i = 0; i < 8; i++)
    if (board[i][y] == 'Q')
      return 0;

  return 1;
}

int
vertical(int x)
{
  int i;

  for (i = 0; i < 8; i++)
	 if (board[x][i] == 'Q')
		return 0;

  return 1;
}

int
diagonal(int x, int y)
{
  int x1, y1;

  for (x1 = x, y1 = y; x1 >= 0 && y1 >= 0; x1--, y1--);

  for (; x1 <= 7 && y1 <= 7; x1++, y1++)
	 if (board[x1][y1] == 'Q')
      return 0;

  for (x1 = x, y1 = y; x1 <= 7 && y1 >= 0; x1++, y1--);

  for (; x1 >= 0 && y1 <= 7; x1--, y1++)
	 if (board[x1][y1] == 'Q')
      return 0;

  return 1;
}

int
puthere(int x, int y)
{
  return (horizontal(y) && vertical(x) && diagonal(x, y));
}

void shift(int y)
{
int i, x=8, shifted=0;

	for(i=0; i<8; i++)
	{
		if(board[i][y]=='Q')
		{
			x=i;
			board[i][y]='.';
		}
	}

	display();

	for(i=x+1; i<8; i++)
		if(puthere(i, y))
		{
			shifted=i;
			board[i][y]='Q';
			break;
		}

	if(shifted==0)
	{
		board[x][y]='Q';
		printf("%d didn't shift\n", y);
	}
	else
		printf("%d shifted to %d\n", y, shifted);
}

void place(void)
{
int i, j;

  for (j = 0; j < 8; j++)
	 for (i = 0; i < 8; i++)
		if (puthere(i, j))
		{
			board[i][j] = 'Q';
			total++;
		}

  display();
}

void
main(void)
{
int i;

  initboard();
  while(total<8)
  {
    place();
    for(i=total-1; i >= 0; i--)
	shift(i);
  }

  display();

  printf("Displaying %d pieces.\n", total);
}
