/*
Dustin Sallings
*/
#include <stdio.h>

char board[8][8];

void
initboard(void)
{
  int i, j;

  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      board[i][j] = '.';
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

  for (x1 = x, y1 = y; x >= 0 && y >= 0; x--, y--);

  for (x1 = x, y1 = y; x <= 7 && y <= 7; x++, y++)
    if (board[x][y] == 'Q')
      return 0;

  for (x1 = x, y1 = y; x <= 7 && y >= 0; x++, y--);

  for (x1 = x, y1 = y; x >= 0 && y <= 7; x--, y++)
    if (board[x][y] == 'Q')
      return 0;

  return 1;
}

int
puthere(int x, int y)
{
  return (horizontal(y) && vertical(x) && diagonal(x, y));
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

void
main(void)
{
  int i, j;

  initboard();

  for (j = 0; j < 8; j++)
    for (i = 0; i < 8; i++)
      if (puthere(i, j))
	board[i][j] = 'Q';

  display();
}
