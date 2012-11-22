#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/glut.h"
#include "fem_draw.h"
#include "fem_file_reader.h"
#include "fem_math.h"
#include "fem_graphics.h"
#include "fem_keyboard.h"


float rotAngle = 0.;
GLuint Window = 0;
int numberOfElms=0;
float scale,**beamCoord, **forceVector, **displacementVector;
int viewState;

// static GLuint TexObj[2];N
GLfloat Angle = 0.0f;

void display(void)
{
  glColor3f(1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glClearColor(0.9, 0.9, 0.9, 0.9);
  drawDiagrams();
  printf("%d\n", viewState);
  switch ( viewState ) {
    case 0:
    drawMomentDiagrams();
    break;
    case 1:
    drawAxialForceDiagrams();
    break;
    case 2:
    drawShearDiagrams();
    break;
    default:
    drawMomentDiagrams();
    break;
  }
  drawElements();
  glFlush();
}

void draw(int argc, char **argv){
  glutInit(&argc, argv);
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(900, 900);
  glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH | GLUT_SINGLE );
  init();
  viewState=1;
  Window = glutCreateWindow("Texture Objects");
  if (!Window)
  {
    exit(1);
  }
  glutReshapeFunc( reshape );
  glutKeyboardFunc (keyboard);
    glutIdleFunc( idle );
  glutDisplayFunc(display);
  glutMainLoop();
}


int main( int argc, char *argv[] )
{
  readFile("temp.dat");
  draw(argc, argv);

  return 0;
}
