#ifndef FEM_GRAPHICS_H
#define FEM_GRAPHICS_H
#include <stdio.h>
#include <GL/glut.h>

extern int viewState;

void init();

void reshape(int, int);
void idle(void);



#endif