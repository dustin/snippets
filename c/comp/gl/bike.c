#define GLSTUFF
#define LOUD
#define LIGHTING
#include <stdio.h>
#include <stdlib.h>
#ifdef GLSTUFF
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>
#endif

#define WEST  0x1
#define NORTH 0x2
#define EAST  0x4
#define SOUTH 0x8

#define DOWN  'v'
#define UP    '^'
#define RIGHT '>'
#define LEFT  '<'

#define EXIT  0
#define WIRE  1
#define MODEL 2

#define ZOOM_IN     0
#define ZOOM_OUT    1
#define ZOOM_NORMAL 2

int map[20][20];
int map2[20][20];
int wire=1, zfactor=0, model=0;
int width, height;
FILE *in;

void
printsq(int i, int j)
{
  putchar('|');
  if (map[i][j] & NORTH)
    putchar(UP);
  else
    putchar(' ');
  if (map[i][j] & EAST)
    putchar(RIGHT);
  else
    putchar(' ');
  if (map[i][j] & SOUTH)
    putchar(DOWN);
  else
    putchar(' ');
  if (map[i][j] & WEST)
    putchar(LEFT);
  else
    putchar(' ');
}

void
printmap(void)
{
  int i, j;

#ifdef GLSTUFF
 glClear(GL_COLOR_BUFFER_BIT);

/*
 if(wire)
	 glBegin(GL_LINE_LOOP);
 else
	 glBegin(GL_POLYGON);

	 glVertex3f(0, 0, 0);
	 glVertex3f(width, 0, 0);
	 glVertex3f(width, height, 0);
	 glVertex3f(0, height, 0);
	 glEnd();

 if(wire)
	 glBegin(GL_LINE_LOOP);
 else
	 glBegin(GL_POLYGON);

	 glVertex3f(width, 0, 0);
	 glVertex3f(0, 0, 0);
	 for(j=0;j<height;j++)
		glVertex3f(j, 0, map2[0][j]);

	 glEnd();
*/
#endif

#ifdef LOUD
  putchar('|');
  for (j = 0; j < (width * 5) - 1; j++)
    putchar('-');
  puts("|");
#endif
  for (i = 0; i < height; i++)
    {
      for (j = 0; j < width; j++)
	{
#ifdef GLSTUFF

 if(wire)
	 glBegin(GL_LINE_LOOP);
 else
	 glBegin(GL_POLYGON);

/*
 * glVertex3f(width-j, height-i, map[height-i][width-j]);
 * glVertex3f(width-j, height-i+1, map[height-i+1][width-j]);
 * glVertex3f(width-j+1, height-i+1, map[height-i+1][width-j+1]);
 * glVertex3f(width-j+1, height-i, map[height-i][width-j+1]);
 */

	  glVertex3f(j, i, map[i][j]);
	  glVertex3f(j, i + 1, map[i + 1][j]);
	  glVertex3f(j + 1, i + 1, map[i + 1][j + 1]);
	  glVertex3f(j + 1, i, map[i][j + 1]);
	  glEnd();
	  glFlush();
#endif
#ifdef LOUD
	  printsq(i, j);
#endif
	}
#ifdef LOUD
      fputs("|\n|", stdout);
      for (j = 0; j < (width * 5) - 1; j++)
	putchar('-');
      puts("|");
#endif
    }
}

#ifdef LOUD
void
printmap2(void)
{
  int i, j;

  for (i = 0; i < height; i++)
    {
      for (j = 0; j < width; j++)
	printf("%d ", map2[i][j]);
      putchar('\n');
    }
}
#endif

void
setroute(int x1, int y1, int x2, int y2)
{
  int i, dir;

  if (x1 == x2)
    {
      if (y1 > y2)
	{
	  dir = NORTH;
	  i = y1 + 1;
	  y1 = y2 + 1;
	  y2 = i;
	}
      else
	dir = SOUTH;
      for (i = y1; i < y2; i++)
	map[i - 1][x1 - 1] |= dir;
    }
  else
    {
      if (x1 > x2)
	{
	  dir = WEST;
	  i = x1 + 1;
	  x1 = x2 + 1;
	  x2 = i;
	}
      else
	dir = EAST;
      for (i = x1; i < x2; i++)
	map[y1 - 1][i - 1] |= dir;
    }
}

void
zoom(int value)
{
	switch(value)
	{
		case ZOOM_IN:
			glTranslatef(0, 0, 5);
			zfactor+=5;
			break;
		case ZOOM_OUT:
			glTranslatef(0, 0, -5);
			zfactor-=5;
			break;
		case ZOOM_NORMAL:
			glTranslatef(0, 0, -zfactor);
			zfactor=0;
			break;
	}
}

#ifdef GLSTUFF
void
display(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glCallList(1);
  printmap();
  glFlush();
  glutSwapBuffers();
}

void
menu(int value)
{
	switch(value)
	{
		case EXIT:
			exit(0);
			break;
		case WIRE:
			if(wire)
				wire=0;
			else
				wire=1;
			break;
		case MODEL:
			if(model)
			{
				model=0;
				glShadeModel(GL_FLAT);
			}
			else
			{
				model=1;
				glShadeModel(GL_SMOOTH);
			}
			break;
	}
	display();
}
#endif

void
init(void)
{
  int i, j, tmp, coord[4];

#ifdef GLSTUFF
#ifdef LIGHTING
  GLfloat light_diffuse[] = {1.0, 0.0, 0.0, 1.0};
  GLfloat light_position[] = {0.0, 0.0, 20.0, 1.0};
  GLfloat shininess[]={40.0};
  GLfloat specular[]={-0.6, 0.6, -0.6, 1};

  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);
  glShadeModel(GL_FLAT);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glMaterialfv(GL_FRONT, GL_SHININESS, shininess);
  glMaterialfv(GL_FRONT, GL_SPECULAR, specular);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);
#endif

  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glColor3f(0.2, 1.0, 0.2);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-1, width+1, -1, height+1, -1, 50);
  glutCreateMenu(menu);
  glutAddMenuEntry("Toggle Wire", WIRE);
  glutAddMenuEntry("Toggle Model", MODEL);
  glutAddMenuEntry("Exit", EXIT);
  glutAttachMenu(GLUT_RIGHT_BUTTON);
  glutCreateMenu(zoom);
  glutAddMenuEntry("Zoom In", ZOOM_IN);
  glutAddMenuEntry("Zoom Out", ZOOM_OUT);
  glutAddMenuEntry("Zoom Normal", ZOOM_NORMAL);
  glutAttachMenu(GLUT_MIDDLE_BUTTON);
/*
  gluPerspective( 1.0, 1.0, 10.0);
*/
  glMatrixMode(GL_MODELVIEW);
  gluLookAt(-1, -1, 25, 0, 0, 0, 0, 1, 0);
#endif

  for (i = 0; i < width; i++)
    for (j = 0; j < height; j++)
      map[i][j] = 0;

  for (i = 0; i < height; i++)
    for (j = 0; j < width; j++)
      {
	fscanf(in, "%d", &tmp);
	map2[i][j] = tmp;
      }

  for (i = 0; i < 4; i++)
    {
      fscanf(in, "%d", &coord[i]);
    }
  while (coord[0] || coord[1] || coord[2] || coord[3])
    {
      setroute(coord[1], coord[0], coord[3], coord[2]);
      for (i = 0; i < 4; i++)
	{
	  fscanf(in, "%d", &coord[i]);
	}
    }
}

void
dosolve(int y1, int x1, int y2, int x2)
{
  printf("Solving from %d,%d to %d,%d\n", y1, x1, y2, x2);
  if (x1 == x2 && y1 == y2)
    printf("To get from %d-%d to %d-%d, stay put!\n", y1, x1, y2, x2);
}

void
solve(void)
{
  int i, coord[4];

  for (i = 0; i < 4; i++)
    {
      fscanf(in, "%d", &coord[i]);
    }
  while (coord[0] || coord[1] || coord[2] || coord[3])
    {
      dosolve(coord[0], coord[1], coord[2], coord[3]);
      for (i = 0; i < 4; i++)
	{
	  fscanf(in, "%d", &coord[i]);
	}
    }
}

void
main(int argc, char *argv[])
{
  if (argc > 1)
    in = fopen(argv[1], "r");
  else
    in = fopen("bike.in", "r");
  fscanf(in, "%d %d", &height, &width);

#ifdef GLSTUFF
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutCreateWindow("Dustin's Map proggy");
  glutDisplayFunc(display);
#endif

  init();
  printmap();
#ifdef LOUD
  printmap2();
#endif
  solve();
#ifdef GLSTUFF
  glutMainLoop();
#endif
}
