#include <stdio.h>
#include <stdlib.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

#include "land.h"

struct config conf={0, 0};

void display(void)
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glCallList(1);
    /*
    glLoadIdentity();
    glRotatef(22.0, 0.0, 1.0, 0.0);
    */
    glutSwapBuffers();
}

void menu(int value)
{
    switch(value)
    {
	case EXIT:
	    exit(0);
	    break;
	case WIRE:
	    if(conf.wire)
		conf.wire=0;
	    else
		conf.wire=1;
	    break;
	case MODEL:
	    if(conf.model==0)
	    {
		conf.model=1;
		glShadeModel(GL_FLAT);
	    }
	    else
	    {
		conf.model=0;
		glShadeModel(GL_SMOOTH);
	    }
	    break;
	case REDRAW:
	    break;
    }
    display();
}

void makelist(l_header head, l_point **points)
{
    int i, j;

    glColor3f(0.4, 1.0, 0.4);
    glNewList(1, GL_COMPILE);

    for(j=0; j<head.height-1; j++)
    {
        glBegin(GL_QUAD_STRIP); 

        glVertex3f(0.0, (float)j, points[j][0].z);
        glVertex3f(0.0, (float)j+1, points[j][0].z);

        for(i=1; i<head.width; i++)
	{
            glVertex3f((float)i, (float)j, points[j][i].z);
            glVertex3f((float)i, (float)j+1, points[j+1][i].z);
	}
        glEnd();
    }
    glEndList();
}

void initcrap(l_header head, l_point **points)
{
    int i;
    GLfloat light_diffuse[] = {0.3, 0.3, 0.3, 0.3};
    GLfloat light_position[] = {0.0, 0.0, 30.0, 1.0};
    GLfloat mat_amb_diff[] = {0.3, 0.7, 0.3, 1.0};
    GLfloat shininess[]={40.0};
    GLfloat specular[]={0.3, 0.3, 0.3, 1};

    light_position[0]=head.width/4;
    light_position[1]=head.height/4;
    light_position[2]=head.maxz+1;

    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
    glLightfv(GL_LIGHT0, GL_POSITION, light_position);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, shininess);
    glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, specular);
    glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE, mat_amb_diff);
    glEnable(GL_DEPTH_TEST);

    glDepthFunc(GL_LEQUAL);

    glClearColor(0.0, 0.0, 0.0, 0.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glColor3f(0.4, 0.4, 1.0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-2, head.width+1, -2, head.height+1, 0-head.maxz, 50);

    glutCreateMenu(menu);
    glutAddMenuEntry("Toggle Wire", WIRE);
    glutAddMenuEntry("Toggle Model", MODEL);
    glutAddMenuEntry("Redraw", REDRAW);
    glutAddMenuEntry("Exit", EXIT);
    glutAttachMenu(GLUT_RIGHT_BUTTON);

    glMatrixMode(GL_MODELVIEW);

    makelist(head, points);
}

void main(int argc, char **argv)
{
    l_header head;
    l_point **points;

    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_DEPTH);
    glutCreateWindow("O View");
    glutDisplayFunc(display);

    points=openfile("blah", &head);

    initcrap(head, points);
    glutMainLoop();
}
