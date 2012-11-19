#include "fem_draw.h"
#include "fem_math.h"




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
	glLineWidth (7.0);

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

void drawMomentDiagrams(void)
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
		xMax = lengthOfVector(pervec)*scale;
		x=0;
		dx=(xMax)/500;
		scaleValue=20000*scale;
		glTranslated(-0.5  , -0.5, 0);
		glTranslated((beamCoord[i][0]), (beamCoord[i][1]), 0);
		glRotatef(vectorRotation(pervec), 0, 0, 0.1);
		glLineWidth(1);
		glBegin(GL_LINES);
		glColor3f (1.0, 0.0, 0.0);
		printf(" xmax = %f | dx =%f | scaleValue = %f | Fy = %f | M = %f | pervec 1 =%f | pervec2= %f \n",xMax, dx, scaleValue,Fy,M,pervec[0],pervec[1]);
		glVertex2d(x/scale, 0);

		for (x;x<=xMax;x+=dx) {
			glColor3f (0.09, 0.18, 0.68);
			glVertex2d(x/scale, -momentFunction(x, M, Fy)/scaleValue);
			glColor3f (0, 0.58, 0.60);
			glVertex2d(x/scale, 0);
		}
		glVertex2d(xMax/scale, 0);
		glEnd();
		glPopMatrix();
	}
}
void drawShearDiagrams(void)
{
	int i;
	float Fy, xMax  ,scaleValue, pervec[2];
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fy=forceVector[i][1]/scale/6;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec);
		glTranslated(-0.5  , -0.5, 0);
		glTranslated((beamCoord[i][0]), (beamCoord[i][1]), 0);
		glRotatef(vectorRotation(pervec), 0, 0, 0.1);
		glLineWidth(2);
		// glColor3f (1.0, 0.0, 0.0);
		printf(" xmax = %f | scaleValue = %f | Fy = %f  | pervec 1 =%f | pervec2= %f \n",xMax,  scaleValue,Fy,pervec[0],pervec[1]);
			glBegin( GL_POLYGON );
			glColor3f(0., 0., 1.);      glVertex2f( 0,Fy);
			glColor3f(0., 0., 1.);      glVertex2f(  xMax,  Fy );
			glColor3f(0., 1., 1.);      glVertex2f(  xMax, 0 );
			glColor3f(0., 1., 1.);      glVertex2f( 0, 0 );
		glVertex2d(xMax/scale, 0);
		glEnd();
		glPopMatrix();
	} 

}

void drawAxialForceDiagrams(void)
{
	int i;
	float Fx, xMax  ,scaleValue, pervec[2];
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fx=forceVector[i][0]/scale/6;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec);
		glTranslated(-0.5  , -0.5, 0);
		glTranslated((beamCoord[i][0]), (beamCoord[i][1]), 0);
		glRotatef(vectorRotation(pervec), 0, 0, 0.1);
		glLineWidth(2);
		// glColor3f (1.0, 0.0, 0.0);
		printf(" xmax = %f | scaleValue = %f | Fx = %f  | pervec 1 =%f | pervec2= %f \n",xMax,  scaleValue,Fx,pervec[0],pervec[1]);
			glBegin( GL_POLYGON );
			glColor3f(0., 0., 1.);      glVertex2f( 0,Fx);
			glColor3f(0., 0., 1.);      glVertex2f(  xMax,  Fx );
			glColor3f(0., 1., 1.);      glVertex2f(  xMax, 0 );
			glColor3f(0., 1., 1.);      glVertex2f( 0, 0 );
		glVertex2d(xMax/scale, 0);
		glEnd();
		glPopMatrix();
	} 

}
