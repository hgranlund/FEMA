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
 #include "math.h"
#include "GL/glut.h"

 static float rotAngle = 0.;
 static GLuint Window = 0;
 static int numberOfElms=0;
 static float scale,**beamCoord, **forceVector, **displacementVector;

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

    glClearColor(0.0, 0.0, 0.0, 0.0);
}

float adjustPoint(float point, float scale ,float offset)
{
    return (point/scale)-offset;
}

float squereF(float x)
{
    return x*x;
}
float momentFunction(float x, float moment, float Fy)
{
    return Fy*x+moment;
}
float getLengthOfVector(float* vec)
{
    return sqrt(squereF(vec[0])+ squereF(vec[1]));
}
float getVectorRotation(float* vec)
{
    // return atan2(vec[1],vec[2])*(180/3.1459);
    float angle = asin(vec[1]/getLengthOfVector(vec))*(180/3.1459);
    if (vec[0]<0){
        angle=180-angle;
    }
    // float angle = acos(vec[0]/getLengthOfVector(vec))*(180/3.1459);
    printf("Vinkel til elent %f\n", angle); 
    return angle;
}

void getPerpendicularUnitVector(float* vec)
{
    printf("%f %f \n", vec[0], vec[1]);
    float  scale, xtemp;
    xtemp=vec[0];
    vec[0]=vec[1];
    vec[1]=-xtemp;
    scale= getLengthOfVector(vec);
    printf("%f", scale);
    vec[0]=vec[0]/scale;
    vec[1]=vec[1]/scale;
    printf("%f %f \n", vec[0], vec[1]);
}


void drawElements(void)
{
    glPushMatrix();
    int i = 0;
    float offsetX,offsetY ;
    offsetX=0;
    offsetY=0;
    // glScalef(0.1,0.1, 1);
    glRotatef(0, 0.0, 0.0, 0.1);
    glTranslated(-0.5  , -0.5, 0);
    glLineWidth (5.0);

    glBegin (GL_LINES);
    glColor3f (0.0, 0.0, 0.0);
    for (i = 0; i < numberOfElms; ++i)
    {
        glVertex2f (beamCoord[i][0],beamCoord[i][1]);
        glVertex2f (beamCoord[i][2], beamCoord[i][3]);
    } 
    glEnd ();
    glPopMatrix();
}

static void drawMomentDiagrams(void)
{
    int i;
    float M,Fy,x, xMax ,dx ,scaleValue, pervec[2];
    for (i = 0; i < numberOfElms; ++i)
    {
        glPushMatrix();
        Fy=forceVector[i][1];
        M=forceVector[i][2];
        pervec[0]=beamCoord[i][2]-beamCoord[i][0];
        pervec[1]=beamCoord[i][3]-beamCoord[i][1];
        xMax = getLengthOfVector(pervec)*scale;
        x=0;
        dx=(xMax)/500;
        scaleValue=20000*scale;
        glTranslated(-0.5  , -0.5, 0);
        glTranslated((beamCoord[i][0]), (beamCoord[i][1]), 0);
        glRotatef(getVectorRotation(pervec), 0, 0, 0.1);
        glLineWidth(2);
        glBegin(GL_LINES);
        glColor3f (1.0, 0.0, 0.0);
        printf(" xmax = %f | dx =%f | scaleValue = %f | Fy = %f | M = %f | pervec 1 =%f | pervec2= %f \n",xMax, dx, scaleValue,Fy,M,pervec[0],pervec[1]);
        glVertex2d(x/scale, 0);

        for (x;x<=xMax;x+=dx) {
            glColor3f (0.1, 0.1, 0.1);
            glVertex2d(x/scale, -momentFunction(x, M, Fy)/scaleValue);
                glColor3f (0.5, 0.5, 0.5);
                glVertex2d(x/scale, 0);
        }
        glVertex2d(xMax/scale, 0);
        glEnd();
        glPopMatrix();
    } 

}



static void display(void)
{
    glColor3f(1.0, 1.0, 1.0);
    glClear(GL_COLOR_BUFFER_BIT);
    glClearColor(0.9, 0.9, 0.9, 0.9);
    drawMomentDiagrams();
    drawElements();
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
    int  i, j;
    scanf("%d %f", &numberOfElms, &scale);
    beamCoord=malloc(numberOfElms * sizeof(float*));
    for (i = 0; i < numberOfElms; i++)
    {
        beamCoord[i]=malloc(4*sizeof(float));
        for ( j = 0; j < 4; j++)
        {
            scanf("%f", &beamCoord[i][j]);
            beamCoord[i][j] =beamCoord[i][j]/scale;
        }
    }

    forceVector=malloc(numberOfElms * sizeof(float*));
    for (i = 0; i < numberOfElms; i++)
    {
        forceVector[i]=malloc(6*sizeof(float));
        for ( j = 0; j < 6; j++)
        {
            scanf("%f", &forceVector[i][j]);
        }
    }

    displacementVector=malloc(numberOfElms * sizeof(float*));
    for (i = 0; i < numberOfElms; i++)
    {
        displacementVector[i]=malloc(6*sizeof(float));
        for ( j = 0; j < 6; j++)
        {
            scanf("%f", &displacementVector[i][j]);
        }
    }

    // for (i = 0; i < numberOfElms; i++){
    //     for (j = 0; j < 6; j++){
    //         printf("| %10.4f ", forceVector[i][j]);
    //     }
    //     printf("\n");
    // }    for (i = 0; i < numberOfElms; i++){
    //     for (j = 0; j < 4; j++){
    //         printf("| %10.4f ", beamCoord[i][j]);
    //     }
    //     printf("\n");
    // }
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