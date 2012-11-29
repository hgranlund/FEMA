#ifndef DRAW_UTIL
#define DRAW_UTIL
#include "GL/glut.h"
#include <string.h>

struct RGB
{
	float r;
	float g;
	float b;
};


void setColor(struct RGB rgb);
void drawString (char *s, float x, float y, struct RGB rgb);
void drawLine(float x1, float y1, float x2, float y2, struct RGB rgb);
void drawDottetLine(float x1, float y1, float x2, float y2, struct RGB rgb);
void drawCircle(float cx, float cy, float r, int segments, struct RGB rgb) ;

// drawSymboleWithCircle draws a circle with a char s in the middle
void drawSymboleWithCircle(float x, float y, char* s, struct RGB rgb);
void drawArrow(float x,float y, float len);

#endif