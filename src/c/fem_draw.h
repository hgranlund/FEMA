
#ifndef FEM_DRAW_H
#define FEM_DRAW_H
#include "GL/glut.h"
#include <stdio.h>
#include <string.h>
#include "math.h"

extern int numberOfElms, numberOfLoads;
extern float scale,**beamCoord, **forceVector, **displacementVector , **loadVector;

float momentFunction(float x, float moment, float Fy);
void drawElements(void);
void drawMomentDiagrams(void);
void drawShearDiagrams(void);
void drawAxialForceDiagrams(void);
void drawDiagramInit(void);
void drawForces(void);
void drawHeader(char* header);
void drawNavigationMeny(void);

#endif
