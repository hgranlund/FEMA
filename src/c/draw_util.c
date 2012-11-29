/*******************************************************************************
 draw_util contains a set of basic drawing functions

 Author: Simen Haugerud Granlund
 Date modified: 29/11/12 
*******************************************************************************/

#include "draw_util.h"
#include "math.h"

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
void drawSymboleWithCircle(float x, float y, char *s, struct RGB rgb)
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