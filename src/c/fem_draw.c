#include "fem_draw.h"
#include "fem_math.h"


struct RGB
{
	float r;
	float g;
	float b;
};

struct RGB black = {0.,0.,0.};

struct RGB colorBlindRYB(float x, float max)
{
	float colordx=max;
	float r=0;
	float g=0;
	float b=0;
	if (x<colordx){
		b=1- (abs(x)/colordx);
	}
	if (abs(colordx-x)<colordx){
		r=1- (abs(colordx-x)/colordx);
	}
	struct RGB rgb ={r,g,b};
	return rgb;
};


void setColor(struct RGB rgb)
{
	glColor3f(rgb.r, rgb.g, rgb.b);
}

void drawString (char *s, float x, float y, struct RGB rgb)
{
	int i;

	glPushAttrib(GL_ENABLE_BIT); 
	setColor(rgb);
	glRasterPos2f(x, y);

	for (i = 0; i < strlen(s); i++)
		glutBitmapCharacter (GLUT_BITMAP_HELVETICA_18, s[i]);
	glPopAttrib();
}


void femScale(void)
{
	glScalef(0.5,0.5, 1);
}

void drawLine(float x1, float y1, float x2, float y2, struct RGB rgb)
{
	glPushAttrib(GL_ENABLE_BIT); 
	setColor(rgb);
	glBegin(GL_LINES);
	glColor3f(rgb.r, rgb.g, rgb.b);      glVertex2f( x1, y1 );
	glColor3f(rgb.r, rgb.g, rgb.b);  glVertex2f(  x2, y2 );
	glEnd();
	glPopAttrib();
}


void drawDottetLine(float x1, float y1, float x2, float y2, struct RGB rgb)
{
	glPushAttrib(GL_ENABLE_BIT); 
	setColor(rgb);
	glLineStipple(1, 0xAAAA);
	glEnable(GL_LINE_STIPPLE);
	glBegin(GL_LINES);
	glVertex2f(x1,y1);
	glVertex2f(x2,y2);
	glEnd();
	glPopAttrib();

}

void drawCircle(float cx, float cy, float r, int segments, struct RGB rgb) 
{ 
	glPushAttrib(GL_ENABLE_BIT); 
	setColor(rgb);
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
void drawSymbole(float x, float y, char *s, struct RGB rgb)
{
	glPushAttrib(GL_ENABLE_BIT); 
	setColor(rgb);
	drawCircle(x+0.02, y+0.01, 0.04, 50,rgb);
	drawString(s, x, y, rgb); 
	glPopAttrib();
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
		// printf(" xmax = %f |pervec 1 =%f | pervec2= %f \n",xMax,pervec[0],pervec[1]);

		drawDottetLine(0, 0, 0, 1, gray);
		// drawDottetLine(xMax/2, 0, xMax/2, 1.5, gray);
		drawDottetLine(xMax, 0, xMax, 1, gray);
		glPopMatrix();

	}
	// drawMomentDiagrams();
	// drawShearDiagrams();
	// drawAxialForceDiagrams();

}


void drawMomentDiagrams(void)
{

	int i;
	float M,Fy,x, xMax ,dx , biggestMoment,  pervec[2], beamRotation;
	biggestMoment=0;
	for (i = 0; i < numberOfElms; ++i)
	{
		Fy=forceVector[i][1];
		if (forceVector[i][3]>Fy){
			Fy=forceVector[i][3];
		}
		M=forceVector[i][2];
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec)*scale;
		if (biggestMoment< abs(momentFunction(xMax, M, Fy))){
			biggestMoment=abs(momentFunction(xMax, M, Fy));
			
		}
	}

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
		beamRotation=vectorRotation(pervec);
		perpendicularUnitVector(pervec);
		femScale();
		glTranslated((beamCoord[i][0]-pervec[0]), (beamCoord[i][1])-pervec[1], 0);
		// glTranslated((beamCoord[i][0]-pervec[0]*1.5), (beamCoord[i][1])-pervec[1]*1.5, 0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		glBegin(GL_LINES);
		setColor(colorBlindRYB( abs(momentFunction(x, M, Fy)),biggestMoment));
		printf(" xmax = %f | dx =%f | scaleValue = %f | Fy = %f | M = %f | pervec 1 =%f | pervec2= %f \n",xMax, dx, biggestMoment,Fy,M,pervec[0],pervec[1]);
		glVertex2d(x/scale, 0);
		for (x;x<=xMax;x+=dx) {
			setColor(colorBlindRYB( abs(momentFunction(x, M, Fy)),biggestMoment));
			glVertex2d(x/scale, -momentFunction(x, M, Fy)/biggestMoment/2);
			glVertex2d(x/scale, 0);
		}
		glEnd();
		// drawSymbole(xMax/scale/2, 0.1, "M",black);
		glVertex2d(xMax/scale, 0);
		drawLine(0,0,xMax/scale,0,black);
		glPopMatrix();
	}
	float ddy=0.15;
	float ddx=0.012;
	drawSymbole(0.7-ddx, -0.6,"M", black);
	drawSymbole(0, 0,"M", black);
	char s[100] ="0.0000" ;
	sprintf(s,"%8e", biggestMoment);
	drawString(s, 0.75+ddx,   -0.65, black);
	sprintf(s,"%8e", 0.000);
	drawString(s,0.75+ddx,   -0.95, black);


	glPushMatrix();
	setColor(colorBlindRYB(biggestMoment,biggestMoment));
	glBegin( GL_POLYGON );
	glVertex2f( 0.7-ddx,-0.8+ddy);
	glVertex2f(  0.7+ddx,   -0.8+ddy);
	setColor(colorBlindRYB(0,biggestMoment));
	glVertex2f(  0.7+ddx, -0.8-ddy );
	glVertex2f( 0.7-ddx, -0.8-ddy );
	glEnd();
	glPopMatrix();

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
		// printf(" xmax = %f |  Fy = %f  | pervec 1 =%f | pervec2= %f \n",xMax,Fy,pervec[0],pervec[1]);
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
		// printf(" xmax = %f| Fx = %f  | pervec 1 =%f | pervec2= %f \n",xMax,Fx,pervec[0],pervec[1]);
		glBegin( GL_POLYGON );
		glColor3f(0., 0., 1.);      glVertex2f( 0,Fx);
		glColor3f(0., 0., 1.);      glVertex2f(  xMax,  Fx );
		glColor3f(0., 1., 1.);      glVertex2f(  xMax, 0 );
		glColor3f(0., 1., 1.);      glVertex2f( 0, 0 );
		glVertex2d(xMax/scale, 0);
		glEnd();
		struct RGB rgb = {0.,0.,0.};
		drawLine(0,0,xMax,0,rgb);
		glColor3f (0.0, 0.0, 0.0);

		glPopMatrix();
	} 

}

