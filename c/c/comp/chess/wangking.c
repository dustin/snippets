#include <stdio.h>

const int v[]={-2, -1, 1, 2, 2, 1, -1, -2};
const int h[]={1, 2, 2, 1, -1, -2, -2, -1};

int knightjump(int board[][8], int row, int col, int step)
{
int i, j, flag, r, c;

	board [row][col]=step;
	if(step==64)
	{
		for(i=0; i<8; i++)
		{
			for(j=0;j<8; j++)
				printf("%3d", board[i][j]);
			puts("");
		}
		return(1);
	}
	else
	{
		flag=0;
		for(i=0;i<8 && !flag; i++)
		{
			r=row+v[i];
			c=col+h[i];
			if(r>=0 && r<8 && c>=0 && c< 8 && board[r][c]==0)
				flag=knightjump(board, r, c, step+1);
		}

		if(!flag) board[row][col]=0;
		return flag;
	}
}

void main(void)
{
int board[8][8];
int i, j;
for(i=0; i<8; i++)
for(j=0; j<8; j++)
board[i][j]=0;

	knightjump(board, 0, 0, 1);

}
