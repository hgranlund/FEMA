#ifndef FEM_MATH_H
#define FEM_MATH_H
#include <math.h>

float squereF(float x);
float momentFunction(float x, float moment, float Fy);
float lengthOfVector(float* vec);
float vectorRotation(float* vec);
void normalize(float *vec);
void perpendicularUnitVector(float* vec);
float min(float x, float y);
float clamp(float value, float lower, float max);

#endif