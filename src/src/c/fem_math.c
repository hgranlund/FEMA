#include "fem_math.h"



float squereF(float x)
{
    return x*x;
}
float momentFunction(float x, float moment, float Fy)
{
    return Fy*x+moment;
}
float lengthOfVector(float* vec)
{
    return sqrt(squereF(vec[0])+ squereF(vec[1]));
}
float vectorRotation(float* vec)
{
    // return atan2(vec[1],vec[2])*(180/3.1459);
    float angle = asin(vec[1]/lengthOfVector(vec))*(180/3.1459);
    if (vec[0]<0){
        angle=180-angle;
    }
    // float angle = acos(vec[0]/getLengthOfVector(vec))*(180/3.1459);
    printf("Vinkel til elent %f\n", angle); 
    return angle;
}

void normalize(float *vec)
{
    float c;
    c = 1.0 / lengthOfVector(vec);
    vec[0] *= c;
    vec[1] *= c;
}

void getPerpendicularUnitVector(float* vec)
{
    printf("%f %f \n", vec[0], vec[1]);
    float  s, xtemp;
    xtemp=vec[0];
    vec[0]=vec[1];
    vec[1]=-xtemp;
    s= lengthOfVector(vec);
    vec[0]=vec[0]/s;
    vec[1]=vec[1]/s;
    printf("%f %f \n", vec[0], vec[1]);
}