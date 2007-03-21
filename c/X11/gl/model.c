#include <GL/glut.h>

GLfloat light_diffuse[] =
{1.0, 0.0, 0.0, 1.0};
GLfloat light_position[] =
{1.0, 1.0, 1.0, 0.0};
GLUquadricObj *qobj;

void 
display(void)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glCallList(1);        /* render sphere display list */
  glutSwapBuffers();
}

void 
gfxinit(void)
{
  qobj = gluNewQuadric();
  gluQuadricDrawStyle(qobj, GLU_FILL);
  glNewList(1, GL_COMPILE);  /* create sphere display list */

  glBegin(GL_POLYGON);
  glVertex2s(1, 1);
  glVertex2s(1,0);
  glVertex2s(0, 1);
  glEnd();

  glEndList();
  /*
  glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, light_position);
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_DEPTH_TEST);
  */
  glMatrixMode(GL_PROJECTION);
  gluPerspective( /* field of view in degree */ 40.0,  /* aspect 
                                                          ratio 
                                                        */ 1.0,
    /* Z near */ 1.0, /* Z far */ 10.0);
  glMatrixMode(GL_MODELVIEW);
  gluLookAt(0.0, 0.0, 5.0,  /* eye is at (0,0,5) */
    0.0, 0.0, 0.0,      /* center is at (0,0,0) */
    0.0, 1.0, 0.);      /* up is in positive Y direction */
  glTranslatef(0.0, 0.0, -1.0);
}

void 
main(int argc, char **argv)
{
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
  glutCreateWindow("Cuz I'm a model");
  glutDisplayFunc(display);
  gfxinit();
  glutMainLoop();
}
