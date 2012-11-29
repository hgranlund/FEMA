
#ifndef FEM_DRAW_H
#define FEM_DRAW_H
#include "GL/glut.h"
#include <stdio.h>
#include <string.h>
#include "math.h"

extern int numberOfElms, numberOfLoads;
extern float scale,**beamCoord, **forceVector, **displacementVector , **loadVector;


void drawFrame(void);
void drawMomentDiagrams(void);
void drawShearDiagrams(void);
void drawAxialForceDiagrams(void);
void drawForces(void);
void drawWindowInit(char* header);

#endif
