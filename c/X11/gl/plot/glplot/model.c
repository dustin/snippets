#include <stdio.h>
#include <GL/glut.h>
#include "data.h"

#define ZOOM_IN 0.1
#define ZOOM_OUT -0.1
#define ZOOM_NONE 0

#define TITLE "Dustin's Land Plotter Thing"

struct glob {
    double max_x;
    double max_y;
    double min_x;
    double min_y;
    double x_diff;
    double y_diff;
    double width;
    double height;
} glob;

void 
display(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glCallList(1);        /* render sphere display list */
  glutSwapBuffers();
}

void 
gfxinit(char *filename)
{
  FILE *f;
  Head h;
  Point p;
  int i;

  f=fopen(filename, "rb");
  if(f==NULL)
  {
     perror(filename);
     exit(1);
  }

  glColor3f(1.0, 1.0, 1.0);

  fread(&h, sizeof(h), 1, f);

  glNewList(1, GL_COMPILE);  /* create sphere display list */

  glBegin(GL_LINE_STRIP);

  for(i=0; i<h.num_points; i++)
  {
      fread(&p, sizeof(p), 1, f);

      if(p.lat >h.max_lat)
      {
          glEnd();
          glBegin(GL_LINE_STRIP);

      }
      else
      {
          glVertex2f(p.lat, p.lng);
      }
  }
  glEnd();

  glEndList();

  printf("%f, %f, %f, %f\n", h.min_lat, h.max_lat, h.min_lng, h.max_lng);
  printf("%f, %f\n",
      (h.min_lat + (h.lat_diff)/2),
      (h.min_lng + (h.lng_diff)/2)
      );

  glob.max_x=h.max_lat;
  glob.max_y=h.max_lng;
  glob.min_x=h.min_lat;
  glob.min_y=h.min_lng;

  glob.x_diff=(h.lat_diff);
  glob.y_diff=(h.lng_diff);

  glMatrixMode(GL_PROJECTION);
  gluPerspective(90.0, 1.0, 1.0, 10.0);
  glMatrixMode(GL_MODELVIEW);

  printf("Lat diff is %f\n", h.lat_diff);

  gluLookAt(
      (h.min_lat + (h.lat_diff)/2),
      (h.min_lng + (h.lng_diff)/2),
      ((h.lat_diff/10)+1),
      (h.min_lat + (h.lat_diff)/2),
      (h.min_lng + (h.lng_diff)/2),
      0.0,
      0.0, 1.0, 0.0);

  glTranslatef(0.0, 0.0, -1.0);
}

void zoom(int x, int y, double dir)
{
double lng, lat;

    lat = (double) (1 - (2*x/glob.width));
    lng = (double) (1 - (2*y/glob.height));

    printf("%f %f\n", lat, lng);

    glTranslatef(lat, -lng, dir);

    display();
}

void buttons(int button, int state, int x, int y)
{

    if(state==1)
        printf("%d, %d, %d, %d\n%fx%f\n", button, state, x, y,
             glob.width, glob.height);

    switch(button)
    {
	case 0:
            if(state==1)
	        zoom(x, y, ZOOM_IN);
            break;

	case 1:
	    if(state==1)
	        zoom(x, y, ZOOM_OUT);
	    break;

        case 2:
	    if(state==1)
		zoom(x, y, ZOOM_NONE);
            break;
    }
}

void reshape(int x, int y)
{
    if(x!=y)
	glutReshapeWindow(y, y);
    glob.width=x;
    glob.height=y;

    glViewport(0,0,x,y);
}

void 
main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutCreateWindow(TITLE);
  glutDisplayFunc(display);
  glutMouseFunc(buttons);
  glutReshapeFunc(reshape);

  gfxinit(argv[1]);
  glutMainLoop();
}
