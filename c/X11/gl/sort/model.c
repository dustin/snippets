/*
 * Copyright (c) 1997  Dustin Sallings
 *
 * $Id: model.c,v 1.1 1997/08/26 06:00:08 dustin Exp $
 */

/*
 * This program models various sorting algorithms.
 */

#include <GL/glut.h>

#define SIZE 100

int a[SIZE];

void initary(void)
{
   int i;

   srand();
   for(i=0; i<SIZE; i++)
   {
      a[i]=(rand()%SIZE);
   }
}

void 
display(void)
{
int i;
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  glBegin(GL_LINES);

  for(i=0; i<SIZE; i++)
  {
     glVertex2s(i, a[i]);
     glVertex2s(i, 0);
  }

  glEnd();

  glFlush();

  glutSwapBuffers();
}

void selection_sort(void)
{
int i, j, min, t;

    for(i=0; i<=SIZE-1; i++)
    {
	min=i;
	for(j=i+1; j<=SIZE; j++)
	{
	    if(a[j]<a[min])
		min=j;
	    t=a[min]; a[min]=a[i]; a[i]=t;
	    display();
	}
    }
}

void insertion_sort()
{
  int i, j, v;

  for(i=1; i<SIZE; i++)
  {
      v=a[i]; j=i;
      while(a[j-1]>v)
      {
	  a[j]=a[j-1]; --j;
      }
      a[j]=v;
      display();
  }
}

void bubble_sort(void)
{
int i, j, t;

  for(i=SIZE-1; i>=0; i--)
  {
      for(j=1; j<=i; j++)
      {
	  if(a[j-1]>a[j])
	  {
	      t=a[j-1]; a[j-1]=a[j]; a[j]=t;
	      display();
	  }
      }
  }
}

void quick_sort(int l, int r)
{
  int i, j, v, t;

  if(r>l)
  {
      v=a[r]; i=l-1; j=r;

      do
      {
	 do{ i++;} while(a[i]<v);
	 do{ j--; } while(a[j]>v);
	 t=a[i]; a[i]=a[j]; a[j]=t;
	 display();
      } while (j>i);

      a[j]=a[i]; a[i]=a[r]; a[r]=t;

      display();

      quick_sort(l, i-1);
      quick_sort(i+1, r);
  }
}

void menu(int which)
{
    switch(which)
    {
	case 1:
		selection_sort(); break;
	case 2:
		insertion_sort(); break;
	case 3:
		bubble_sort(); break;
	case 4:
		quick_sort(0, SIZE); break;
	case 98:
		initary(); display(); break;
	case 99:
		exit(0); break;
    }
}

void 
gfxinit(void)
{
  glMatrixMode(GL_PROJECTION);
  gluPerspective( 170.0, 1.0, 5.0, 100.0);
  glMatrixMode(GL_MODELVIEW);
#if(0)
  gluLookAt(0.0, 0.0, 5.0,  /* eye is at (0,0,50) */
    0.0, 0.0, 0.0,      /* center is at (0,0,0) */
    0.0, 1.0, 0.);      /* up is in positive Y direction */
#endif

  glTranslatef(-50.0, -50.0, -5.0);

  glutCreateMenu(menu);
  glutAddMenuEntry("Selection", 1);
  glutAddMenuEntry("Insertion", 2);
  glutAddMenuEntry("Bubble", 3);
  glutAddMenuEntry("Quicksort", 4);
  glutAddMenuEntry("New Array", 98);
  glutAddMenuEntry("Exit", 99);
  glutAttachMenu(GLUT_RIGHT_BUTTON);
}

void 
main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutCreateWindow("Cuz I'm a model");
  glutDisplayFunc(display);
  initary();
  gfxinit();
  glutMainLoop();
}
