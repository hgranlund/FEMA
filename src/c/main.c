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
int numberOfLoads=0;
float scale,**beamCoord, **forceVector, **displacementVector, **loadVector;
int viewState;

// static GLuint TexObj[2];N
GLfloat Angle = 0.0f;

void display(void)
{
  glColor3f(1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glClearColor(0.9, 0.9, 0.9, 0.9);
  switch ( viewState ) {
    case 0:
    drawMomentDiagrams();
    drawDiagrams();
    drawHeader("Moment Diagrams");
    break;
    case 1:
    drawAxialForceDiagrams();
    drawDiagrams();
    drawHeader("Normal Force Diagrams");
    break;
    case 2:
    drawShearDiagrams();
    drawDiagrams();
    drawHeader("Shear Force Diagrams");
    break;
    case 3:
    drawForces();
    drawHeader("Initial State");
    break;
    case 4:
    drawHeader("The Frame");
    break;
    default:
    break;
  }
  drawElements();
  drawNavigationMeny();
  glFlush();
}

void draw(int argc, char **argv){
  glutInit(&argc, argv);
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(1000, 900);
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
  readFile("FortranOutput.dat");
  draw(argc, argv);

  return 0;
}
