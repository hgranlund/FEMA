#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GL/glut.h"
#include "fem_draw.h"
#include "fem_file_reader.h"
#include "fem_keyboard.h"


float rotAngle = 0.;
GLuint Window = 0;
int numberOfElms=0;
int numberOfLoads=0;
float scale,**beamCoord, **forceVector, **displacementVector, **loadVector;
int viewState, currentViewState;


// static GLuint TexObj[2];N
GLfloat Angle = 0.0f;

void init(void)
{

    GLfloat values[2];
    glGetFloatv (GL_LINE_WIDTH_GRANULARITY, values);
    glGetFloatv (GL_LINE_WIDTH_RANGE, values);
    glEnable (GL_LINE_SMOOTH);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glHint (GL_LINE_SMOOTH_HINT, GL_DONT_CARE);

    glClearColor(0.0, 0.0, 0.0, 0.0);
}

void reshape( int w, int h )
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

void idle( void )
{
    if (currentViewState != viewState){
        currentViewState=viewState;
        glutPostRedisplay();
    }
}


void display(void)
{
  glColor3f(1.0, 1.0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glClearColor(0.9, 0.9, 0.9, 0.9);
  switch ( viewState ) {
    case 0:
    drawMomentDiagrams();
    drawDiagramInit();
    drawHeader("Moment Diagrams");
    break;
    case 1:
    drawAxialForceDiagrams();
    drawDiagramInit();
    drawHeader("Axial Force Diagrams");
    break;
    case 2:
    drawShearDiagrams();
    drawDiagramInit();
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

void InstantiateGlut(int argc, char **argv){
  glutInit(&argc, argv);
  glutInitWindowPosition(0, 0);
  glutInitWindowSize(1000, 900);
  glutInitDisplayMode( GLUT_RGB | GLUT_DEPTH | GLUT_SINGLE );
  init();
  viewState=3;
  currentViewState=3;
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
  InstantiateGlut(argc, argv);

  return 0;
}
