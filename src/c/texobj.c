//Compile :
// gcc -I/usr/X11R6/include texobj.c -lglut -lGLU -lGL -lm -o texobj


/*a
 * Example of using the 1.1 texture object functions.
 * Also, this demo utilizes Mesa's fast texture map path.
 *
 * Brian Paul   June 1996   This file is in the public domain.
 */

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/glut.h"

 static float rotAngle = 0.;
 static GLuint Window = 0;
 static float **beamCoord;
// static GLuint TexObj[2];
 static GLfloat Angle = 0.0f;
// static GLboolean UseObj = GL_FALSE;
// static float beamCoor[2][8]={{0,0,0.1,0,0.1,2,0,2},{0,0,-0.1,0,-0.1,-2,-0,-2}};

// static void draw( void )
// {
//   glClear( GL_COLOR_BUFFER_BIT );
//   glColor3f( 1.0, 1.0, 1.0 );

//   /* draw first polygon */
//   // glTranslatef( -1.0, 0.0, 0.0 );
//   glRotatef( 0, 0.0, 0.0, 1.0 );
//   int i;
//   for (i=0; i < 2; ++i) {
//   glPushMatrix();
//   glBegin( GL_POLYGON );
//   glColor3f(1., 0., 0.);      glVertex2f( beamCoor[i][0], beamCoor[i][1] );
//   glColor3f(1., 0., 0.);      glVertex2f(  beamCoor[i][2], beamCoor[i][3] );
//   glColor3f(0., 0., 1.);      glVertex2f(  beamCoor[i][4],  beamCoor[i][5]);
//   glColor3f(0., 0., 1.);      glVertex2f( beamCoor[i][6],  beamCoor[i][7]);
//   glEnd();
//   glPopMatrix();
//   glutSwapBuffers();
//   }
// }

 static void init(void)
 {
    GLfloat values[2];
    glGetFloatv (GL_LINE_WIDTH_GRANULARITY, values);
    printf ("GL_LINE_WIDTH_GRANULARITY value is %3.1f\n", values[0]);

    glGetFloatv (GL_LINE_WIDTH_RANGE, values);
    printf ("GL_LINE_WIDTH_RANGE values are %3.1f %3.1f\n",
        values[0], values[1]);

    glEnable (GL_LINE_SMOOTH);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glHint (GL_LINE_SMOOTH_HINT, GL_DONT_CARE);
    glLineWidth (1.5);

    glClearColor(0.0, 0.0, 0.0, 0.0);
}

static void display(void)
{
    glClear(GL_COLOR_BUFFER_BIT);
    glClearColor(1.0, 0.0, 0.0, 0.0);
    int i = 0;
    for (i = 0; i < 6; ++i)
    {
        glColor3f (0.0, 1.0, 0.0);
        glPushMatrix();
        glRotatef(-rotAngle, 0.0, 0.0, 0.1);
        glBegin (GL_LINES);
        glVertex2f (beamCoord[i][0], beamCoord[i][1]);
        glVertex2f (beamCoord[i][2], beamCoord[i][3]);
        glEnd ();
        glPopMatrix();
    }

    glFlush();
}



static void idle( void )
{
    static double t0 = -1.;
    double dt, t = glutGet(GLUT_ELAPSED_TIME) / 1000.0;
    if (t0 < 0.0)
        t0 = t;
    dt = t - t0;
    t0 = t;
    Angle += 120.0 * dt;
    glutPostRedisplay();
}



/* change view Angle, exit upon ESC */
static void key(unsigned char k, int x, int y)
{
    (void) x;
    (void) y;
    switch (k)
    {
        case 27:
        glutDestroyWindow(Window);
        exit(0);
    }
}



/* new window size or exposure */
static void reshape( int w, int h )
{
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    if (w <= h)
        gluOrtho2D (-1.0, 1.0,
            -1.0 * (GLfloat)h / (GLfloat)w, 1.0 * (GLfloat)h / (GLfloat)w);
    else
        gluOrtho2D (-1.0 * (GLfloat)w / (GLfloat)h,
            1.0 * (GLfloat)w / (GLfloat)h, -1.0, 1.0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

//./texobj -geometry 100x100

void keyboard(unsigned char key, int x, int y)
{
    switch (key)
    {
        case 'r':
        case 'R':
        rotAngle += 20.;
        if (rotAngle >= 360.) rotAngle = 0.;
        glutPostRedisplay();
        break;
    case 27:  /*  Escape Key  */
        exit(0);
        break;
        default:
        break;
    }
}

static void readInput(void)
{
    int numberOfElm, i, j;
    scanf("%d", &numberOfElm);
    beamCoord=malloc(numberOfElm * sizeof(float*));
    for (i = 0; i < numberOfElm; i++)
    {
        beamCoord[i]=malloc(4*sizeof(float));
        for ( j = 0; j < 4; j++)
        {
            scanf("%f", &beamCoord[i][j]);
        }
    }
    for (i = 0; i < numberOfElm; i++){
        for (j = 0; j < 4; j++)
            printf("| %10.4f ", beamCoord[i][j]);
        printf("\n");
    }
}


int main( int argc, char *argv[] )
{
    glutInit(&argc, argv);
    glutInitWindowPosition(0, 0);
    glutInitWindowSize(700, 700);
    glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH | GLUT_SINGLE );

    init();
    readInput();
    Window = glutCreateWindow("Texture Objects");
    if (!Window)
    {
        exit(1);
    }
    glutReshapeFunc( reshape );
    glutKeyboardFunc( key );
    glutKeyboardFunc (keyboard);

    // glutIdleFunc( idle );
    glutDisplayFunc(display);
    glutMainLoop();
    return 0;
}