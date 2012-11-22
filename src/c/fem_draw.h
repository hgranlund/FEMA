
#ifndef FEM_DRAW_H
#define FEM_DRAW_H
#include "GL/glut.h"
#include <stdio.h>
#include <string.h>

extern int numberOfElms;
extern float scale,**beamCoord, **forceVector, **displacementVector;

float momentFunction(float x, float moment, float Fy);
void drawElements(void);
void drawMomentDiagrams(void);
void drawShearDiagrams(void);
void drawAxialForceDiagrams(void);
void drawDiagrams(void);

#endif
