#ifndef FEM_FILE_READER_H
#define FEM_FILE_READER_H

extern int numberOfElms, numberOfLoads;
extern float scale,**beamCoord, **forceVector, **displacementVector, **loadVector;

void readInput(char *filename);


#endif