#include <GL/gl.h>
#include <stdlib.h>
#include <aux.h>

int main(int argc, char** argv)
{
	auxInitDisplayMode (AUX_SINGLE | AUX_RGB);
	auxInitPosition (0, 0, 500, 500);
	auxInitWindow (argv[0]);

	glClearColor (0.0, 0.0, 0.0, 0.0);
	glClear(GL_COLOR_BUFFER_BIT);
	glColor3f(1.0, 1.0, 1.0);
	glMatrixMode (GL_PROJECTION);
	glLoadIdentity ();
	glOrtho(-1.0, 1.0, -1.0, 1.0, -1.0, 1.0);
/*
	glBegin(GL_POLYGON);
		glVertex2f(-0.5, -0.5);
		glVertex2f(-0.5, 0.5);
		glVertex2f(0.5, 0.5);
		glVertex2f(0.5, -0.5);
	glEnd();
*/
	glBegin(GL_POLYGON);
		glVertex2f(0, .85);
		glVertex2f(-.75, -.55);
		glVertex2f(.75, -.55);
	glEnd();
	glBegin(GL_POLYGON);
		glVertex2f(0, -.85);
		glVertex2f(.75, .55);
		glVertex2f(-.75, .55);
	glEnd();
	glFlush();
	sleep (10);
}
