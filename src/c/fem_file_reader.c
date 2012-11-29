#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "fem_file_reader.h"



void readFile(char *filename)
{
    FILE *file;
    file = fopen(filename, "r");

    if (file != NULL){
        printf("leser fil: %s\n", filename);
        int  i, j;
        fscanf(file ,"%d %d %f", &numberOfElms, &numberOfLoads, &scale);
        beamCoord=malloc(numberOfElms * sizeof(float*));
        for (i = 0; i < numberOfElms; i++)
        {
            beamCoord[i]=malloc(4*sizeof(float));
            for ( j = 0; j < 4; j++)
            {
                fscanf(file,"%f", &beamCoord[i][j]);
                beamCoord[i][j] =beamCoord[i][j]/scale-0.5;
            }
        }

        forceVector=malloc(numberOfElms * sizeof(float*));
        for (i = 0; i < numberOfElms; i++)
        {
            forceVector[i]=malloc(6*sizeof(float));
            for ( j = 0; j < 6; j++)
            {
                fscanf(file,"%f", &forceVector[i][j]);
            }
        }

        displacementVector=malloc(numberOfElms * sizeof(float*));
        for (i = 0; i < numberOfElms; i++)
        {
            displacementVector[i]=malloc(6*sizeof(float));
            for ( j = 0; j < 6; j++)
            {
                fscanf(file,"%f", &displacementVector[i][j]);
            }
        }
        loadVector=malloc(numberOfLoads * sizeof(float*));
        for (i = 0; i < numberOfLoads; i++)
        {
            loadVector[i]=malloc(6*sizeof(float));
            for ( j = 0; j < 6; j++)
            {
                fscanf(file,"%f", &loadVector[i][j]);
            }
            loadVector[i][1]=loadVector[i][1]/scale;
            loadVector[i][2]=loadVector[i][2]/scale;
        }
    }
    else {
        printf("Could not open file: %s: \n", filename);
        exit(-1);
    }

}
