#ifndef FEM_FILE_READER_H
#define FEM_FILE_READER_H

extern int numberOfElms;
extern float scale,**beamCoord, **forceVector, **displacementVector;

void readInput(char *filename);


#endif