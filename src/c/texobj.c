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

 static GLuint Window = 0;

 // static GLuint TexObj[2];
 static GLfloat Angle = 0.0f;
 // static GLboolean UseObj = GL_FALSE;
 static float beamCoor[2][8]={{0,0,0.1,0,0.1,2,0,2},{0,0,-0.1,0,-0.1,-2,-0,-2}};

 static void draw( void )
 {
   glClear( GL_COLOR_BUFFER_BIT );
   glColor3f( 1.0, 1.0, 1.0 );

   /* draw first polygon */
   // glTranslatef( -1.0, 0.0, 0.0 );
   glRotatef( 0, 0.0, 0.0, 1.0 );
   int i;
   for (i=0; i < 2; ++i) {
   glPushMatrix();
   glBegin( GL_POLYGON );
   glColor3f(1., 0., 0.);      glVertex2f( beamCoor[i][0], beamCoor[i][1] );
   glColor3f(1., 0., 0.);      glVertex2f(  beamCoor[i][2], beamCoor[i][3] );
   glColor3f(0., 0., 1.);      glVertex2f(  beamCoor[i][4],  beamCoor[i][5]);
   glColor3f(0., 0., 1.);      glVertex2f( beamCoor[i][6],  beamCoor[i][7]);
   glEnd();
   glPopMatrix();
   glutSwapBuffers();
   }  
 }



 static void idle( void )
 {
  static double t0 = -1.;
  double dt, t = glutGet(GLUT_ELAPSED_TIME) / 1000.0;
  if (t0 < 0.0)
    t0 = t;
  dt = t - t0;
  t0 = t;
  Angle += 120.0*dt;
  glutPostRedisplay();
}



/* change view Angle, exit upon ESC */
static void key(unsigned char k, int x, int y)
{
 (void) x;
 (void) y;
 switch (k) {
   case 27:
   glutDestroyWindow(Window);
   exit(0);
 }
}



/* new window size or exposure */
static void reshape( int width, int height )
{
 glViewport(0, 0, (GLint)width, (GLint)height);
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity();
   /*   glOrtho( -3.0, 3.0, -3.0, 3.0, -10.0, 10.0 );*/
 glFrustum( -2.0, 2.0, -2.0, 2.0, 6.0, 20.0 );
   //glPerspective( -2.0, 2.0, -2.0, 2.0, 6.0, 20.0 );
 glMatrixMode(GL_MODELVIEW);

 glLoadIdentity();
 glTranslatef( 0.0, 0.0, -8.0 );
}

//./texobj -geometry 100x100

int main( int argc, char *argv[] )
{
 glutInit(&argc, argv);
 glutInitWindowPosition(0, 0);
 glutInitWindowSize(700, 700);
 glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE );

 Window = glutCreateWindow("Texture Objects");
 if (!Window) {
  exit(1);
}

glutReshapeFunc( reshape );
glutKeyboardFunc( key );
glutIdleFunc( idle );
glutDisplayFunc( draw );
glutMainLoop();
return 0;
}