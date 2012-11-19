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
        fscanf(file ,"%d %f", &numberOfElms, &scale);
        beamCoord=malloc(numberOfElms * sizeof(float*));
        for (i = 0; i < numberOfElms; i++)
        {
            beamCoord[i]=malloc(4*sizeof(float));
            for ( j = 0; j < 4; j++)
            {
                fscanf(file,"%f", &beamCoord[i][j]);
                beamCoord[i][j] =beamCoord[i][j]/scale;
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
        // for (i = 0; i < numberOfElms; i++){
        //     for (j = 0; j < 6; j++){
        //         printf("| %10.4f ", forceVector[i][j]);
        //     }
        //     printf("\n");
        // }    for (i = 0; i < numberOfElms; i++){
        //     for (j = 0; j < 4; j++){
        //         printf("| %10.4f ", beamCoord[i][j]);
        //     }
        //     printf("\n");
        // }
    }
    else {
        perror("kunne ikke åpne fil: \n");
    }

}
