#include "fem_draw.h"
#include "fem_math.h"


struct RGB
{
	float r;
	float g;
	float b;
};


void drawString (void * font, char *s, float x, float y, float z){
	int i;
	glPushAttrib(GL_ENABLE_BIT); 
	glRasterPos3f(x, y, z);

	for (i = 0; i < strlen(s); i++)
		glutBitmapCharacter (font, s[i]);
	glPopAttrib();
}

void femScale(void)
{
	glScalef(0.4,0.4, 1);
}

void drawLine(float x1, float y1, float x2, float y2, struct RGB rgb)
{
	glPushAttrib(GL_ENABLE_BIT); 
	glBegin(GL_LINES);
	glColor3f(rgb.r, rgb.g, rgb.b);      glVertex2f( x1, y1 );
	glColor3f(rgb.r, rgb.g, rgb.b);  glVertex2f(  x2, y2 );
	glEnd();
	glPopAttrib();
}

void drawDottetLine(float x1, float y1, float x2, float y2, struct RGB rgb)
{
	glPushAttrib(GL_ENABLE_BIT); 
	glColor3f(rgb.r, rgb.g, rgb.b);
	glLineStipple(1, 0xAAAA);
	glEnable(GL_LINE_STIPPLE);
	glBegin(GL_LINES);
	glVertex2f(x1,y1);
	glVertex2f(x2,y2);
	glEnd();
	glPopAttrib();

	// float dx,dy,x,y;
	// x=x1;
	// y=y1;
	// dx=0.1;
	// if(x2-x1<0)
	// {
	// 	dx=-0.1;
	// 	x2=x1;
	// }
	// dy=0.1;
	// if(y2-y1<0)
	// {
	// 	dy=-0.1;
	// 	y2=y1;
	// }
	// do
	// {
	// 	printf("%f\n", x);
	// 	drawLine(x, y, x+dx, y+dy,rgb);
	// 	x+=dx;
	// 	y+=dy;
	// 	printf("%f\n", x);

	// } while (x<x2 && y<y2);
}

void drawCircle(float cx, float cy, float r, int segments) 
{ 
	float theta = (2 * 3.1415/ (float)segments); 
	float c = cosf(theta);
	float s = sinf(theta);
	float t;
	int i;
	float x = r;
	float y = 0; 

	glBegin(GL_LINE_LOOP); 
	for(i = 0; i < segments; i++) 
	{ 
		glVertex2f(x + cx, y + cy); 
		t = x;
		x = c * x - s * y;
		y = s * t + c * y;
	} 
	glEnd(); 
}

void drawElements(void)
{
	glPushMatrix();
	int i = 0;
	float lineWith=7;
	femScale();
	glRotatef(0, 0.0, 0.0, 0.1);
	glLineWidth (lineWith);
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

void drawDiagrams(void)
{
	int i;
	float xMax, pervec[2], beamRotation;
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec);
		beamRotation=vectorRotation(pervec);
		perpendicularUnitVector(pervec);
		femScale();
		glTranslated((beamCoord[i][0]), (beamCoord[i][1]),0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		struct RGB gray = {0.5,0.5,0.5};
		printf(" xmax = %f |pervec 1 =%f | pervec2= %f \n",xMax,pervec[0],pervec[1]);

		drawDottetLine(0, 0, 0, 1.5, gray);
		drawDottetLine(xMax/2, 0, xMax/2, 1.5, gray);
		drawDottetLine(xMax, 0, xMax, 1.5, gray);
		glPopMatrix();

	}
	drawMomentDiagrams();
	drawShearDiagrams();
	drawAxialForceDiagrams();

}


void drawMomentDiagrams(void)
{

	int i;
	float M,Fy,x, xMax ,dx ,scaleValue, pervec[2], beamRotation;
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
		beamRotation=vectorRotation(pervec);
		perpendicularUnitVector(pervec);
		femScale();
		glTranslated((beamCoord[i][0]-pervec[0]*1.5), (beamCoord[i][1])-pervec[1]*1.5, 0);
		glRotatef(beamRotation, 0, 0, 0.1);
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
		struct RGB rgb = {0.,0.,0.};
		drawLine(0,0,xMax/scale,0,rgb);
		glPopMatrix();
	}
}
void drawShearDiagrams(void)
{
	int i;
	float Fy, xMax  ,beamRotation, pervec[2];
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fy=forceVector[i][1]/scale/10;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec);
		beamRotation=vectorRotation(pervec);
		femScale();
		perpendicularUnitVector(pervec);
		glTranslated((beamCoord[i][0]-pervec[0]), (beamCoord[i][1])-pervec[1], 0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		printf(" xmax = %f |  Fy = %f  | pervec 1 =%f | pervec2= %f \n",xMax,Fy,pervec[0],pervec[1]);
		glBegin( GL_POLYGON );
		glColor3f(0., 0., 1.);      glVertex2f( 0,Fy);
		glColor3f(0., 0., 1.);      glVertex2f(  xMax,  Fy );
		glColor3f(0., 1., 1.);      glVertex2f(  xMax, 0 );
		glColor3f(0., 1., 1.);      glVertex2f( 0, 0 );
		glVertex2d(xMax/scale, 0);
		glEnd();
		struct RGB rgb = {0.,0.,0.};
		drawLine(0,0,xMax,0,rgb);
		glPopMatrix();
	} 

}

void drawAxialForceDiagrams(void)
{
	int i;
	float Fx, xMax  ,beamRotation, pervec[2];
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fx=forceVector[i][0]/scale/10;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		beamRotation=vectorRotation(pervec);
		xMax = lengthOfVector(pervec);
		perpendicularUnitVector(pervec);
		femScale();
		glTranslated((beamCoord[i][0]-pervec[0]*0.4), (beamCoord[i][1])-pervec[1]*0.4, 0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		// glColor3f (1.0, 0.0, 0.0);
		printf(" xmax = %f| Fx = %f  | pervec 1 =%f | pervec2= %f \n",xMax,Fx,pervec[0],pervec[1]);
		glBegin( GL_POLYGON );
		glColor3f(0., 0., 1.);      glVertex2f( 0,Fx);
		glColor3f(0., 0., 1.);      glVertex2f(  xMax,  Fx );
		glColor3f(0., 1., 1.);      glVertex2f(  xMax, 0 );
		glColor3f(0., 1., 1.);      glVertex2f( 0, 0 );
		glVertex2d(xMax/scale, 0);
		glEnd();
		struct RGB rgb = {0.,0.,0.};
		drawLine(0,0,xMax,0,rgb);
		glPopMatrix();
	} 
	glColor3f (0.0, 0.0, 0.0);
	drawCircle(0.006, 0.02, 0.05, 100);
	drawString(GLUT_BITMAP_HELVETICA_18, "N", -0.01, 0, 0); 

}

