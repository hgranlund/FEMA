#include "fem_draw.h"
#include "fem_math.h"
#

struct RGB
{
	float r;
	float g;
	float b;
};

struct RGB black = {0.,0.,0.};
struct RGB gray = {0.5,0.5,0.5};
struct RGB red = {1.,0.,0.};


struct RGB colorBlindRYB(float x, float maxValue)
{
	float value = 4 * x/maxValue;
	float r   = clamp(min(value - 1.0, -value + 4.5) ,0,1);
	float g = clamp(min(value - 0.5, -value + 3.5), 0,1);
	float b  = clamp(min(value + 0.5, -value + 2.5),0,1);
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

void drawArrow(float x,float y, float len)
{

	float  lengthOfHead;
	lengthOfHead= 0.05;
	glBegin(GL_LINES);
	glVertex2d(x,y);
	glVertex2d(x,y+len);
	glEnd();
	glBegin(GL_TRIANGLES);
	glVertex2d(x,y);
	glVertex2d(x-lengthOfHead,y+lengthOfHead);
	glVertex2d(x+lengthOfHead,y+lengthOfHead);
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

void drawColorBar(float x, float y,float biggestvalue, char* Type)
{
	int i;
	float dl;
	float ddy=0.3;
	float ddx=0.024;
	glPopMatrix();
	drawSymbole(x-0.01, y + 0.05,Type, black);
	char s[100] ="0.0000" ;
	sprintf(s,"%8e", biggestvalue);
	drawString(s, x+ddx+0.03,  y-0.01, black);
	sprintf(s,"%8e", 0.000);
	y=y-ddy;
	drawString(s,x+ddx+0.03, y, black);
	setColor(colorBlindRYB(biggestvalue,biggestvalue));
	glBegin( GL_LINES );
	for (i = 1; i < 500; i++)
	{
		dl = (float)i/500.0;
		setColor(colorBlindRYB(dl,1));
		glVertex2f( x,y+ddy*dl);
		glVertex2f(  x+ddx,y+ddy*dl);
	}
	glEnd();
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
		drawDottetLine(0, 0, 0, 1, gray);
		drawDottetLine(xMax, 0, xMax, 1, gray);
		glLineWidth(2);
		drawDottetLine(-0.1, 1, xMax+0.1, 1, black);
		glPopMatrix();
	}
}

void drawHeader(char* header){
	glPushMatrix();
	drawString(header, -0.2, 0.9, black);
	glPopMatrix();

}

void drawNavigationMeny()
{
	glPushMatrix();
	drawString("Key Functions: ",  -0.95, -0.8, black);
	drawString("Initial State: 'I'    |     Frame: 'F'      |      Moment Diagrams: 'M'" ,  -0.95, -0.9, black);
	drawString("Shear Force Diagrams: 'S'    |     Axial Force Diagrams: 'A'",  -0.95, -0.95, black);
	glEnd();
}

void drawForces(void){
	int i, dof;
	float  x,y;
	char s[100] ="0.0000" ;

	for (i = 0; i < numberOfLoads; ++i)
	{
		y=loadVector[i][2]-0.5;
		x=loadVector[i][1]-0.5;
		glPushMatrix();
		setColor(red);
		glLineWidth(1);
		femScale();
		glTranslatef(x,y +0.03, 0);
		dof= loadVector[i][0];
		switch (dof)
		{
			case 1:
			glRotatef(90, 0, 0, 0.1);
			drawArrow(0, 0, 0.3);
			break;
			case 2:
			drawArrow(0, 0, 0.3);
			break;
			case 3:
			glLineWidth(1);
			glRotatef(90, 0, 0, 0.1);
			drawCircle(0, 0.05, 0.08, 50, black);
			drawArrow(0.08, 0, 0);
			default:
			break;
		}
		sprintf(s,"%5F %s", loadVector[i][3], "N");
		drawString(s, 0.05, 0.05, black);
		glPopMatrix();
	}
}


void drawMomentDiagrams(void)
{

	int i;
	float M,Fy,x, xMax ,dx , biggestMoment,  pervec[2], beamRotation;
	biggestMoment=0;
	for (i = 0; i < numberOfElms; ++i)
	{
		Fy=forceVector[i][1];
		M=forceVector[i][2];
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec)*scale;
		if (biggestMoment< abs(momentFunction(xMax, M, Fy))){
			biggestMoment=abs(momentFunction(xMax, M, Fy));
		}	
		if (biggestMoment< abs(momentFunction(0, M, Fy))){
			biggestMoment=abs(momentFunction(0, M, Fy));
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
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		glBegin(GL_LINES);
		glVertex2d(x/scale, 0);
		for (x;x<=xMax;x+=dx) {
			setColor(colorBlindRYB( abs(momentFunction(x, M, Fy)),biggestMoment));
			glVertex2d(x/scale, -momentFunction(x, M, Fy)/biggestMoment/2);
			glVertex2d(x/scale, 0);
		}
		glEnd();
		glPopMatrix();
	}
	drawSymbole(-0.9, 0.9,"M", black);
	drawColorBar(0.65, -0.65, biggestMoment, "M");
}

void drawShearDiagrams(void)
{
	int i,y;
	float Fy, xMax  ,beamRotation, pervec[2], biggestShear,scaleFactor;
	biggestShear=0;
	for (i = 0; i < numberOfElms; ++i)
	{
		if (abs(forceVector[i][1])>abs(biggestShear)){
			biggestShear=forceVector[i][1];
		}
	}
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fy=forceVector[i][4]/biggestShear/4;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		xMax = lengthOfVector(pervec);
		beamRotation=vectorRotation(pervec);
		femScale();
		perpendicularUnitVector(pervec);
		glTranslated((beamCoord[i][0]-pervec[0]), (beamCoord[i][1])-pervec[1], 0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		glBegin( GL_LINES );
		for (y=0;y<=500;y++) {
			scaleFactor=(float)y/500;
			setColor(colorBlindRYB( abs(forceVector[i][4]*scaleFactor),biggestShear));
			glVertex2d(0, Fy*scaleFactor);
			glVertex2d(xMax, Fy*scaleFactor);
		}
		glEnd();
		glPopMatrix();
	} 
	drawSymbole(-0.9, 0.9,"S", black);
	drawColorBar(0.65, -0.65, biggestShear, "S");

}

void drawAxialForceDiagrams(void)
{
	int i,y;
	float Fx, xMax  ,beamRotation, pervec[2],biggestNormal,scaleFactor;
	biggestNormal=0;
	for (i = 0; i < numberOfElms; ++i)
	{
		if (abs(forceVector[i][0])>biggestNormal){
			biggestNormal=abs(forceVector[i][0]);
		}
	}
	for (i = 0; i < numberOfElms; ++i)
	{
		glPushMatrix();
		Fx=forceVector[i][0]/biggestNormal/4;
		pervec[0]=beamCoord[i][2]-beamCoord[i][0];
		pervec[1]=beamCoord[i][3]-beamCoord[i][1];
		beamRotation=vectorRotation(pervec);
		xMax = lengthOfVector(pervec);
		perpendicularUnitVector(pervec);
		femScale();
		glTranslated((beamCoord[i][0]-pervec[0]), (beamCoord[i][1])-pervec[1], 0);
		glRotatef(beamRotation, 0, 0, 0.1);
		glLineWidth(1);
		glBegin( GL_LINES );
		for (y=0;y<=500;y++) {
			scaleFactor=(float)y/500;
			setColor(colorBlindRYB( abs(forceVector[i][0]*scaleFactor),biggestNormal));
			glVertex2d(0, Fx*scaleFactor);
			glVertex2d(xMax, Fx*scaleFactor);
		}
		glEnd();
		glPopMatrix();
	} 
	drawSymbole(-0.9, 0.9,"N", black);
	drawColorBar(0.65, -0.65, biggestNormal, "N");

}

