#include "fem_keyboard.h"
#include <stdio.h>

void keyboard(unsigned char key, int x, int y)
{

    switch (key)
    {
//         case 'r':
//         case 'R':
//         rotAngle += 20.;
//         if (rotAngle >= 360.) rotAngle = 0.;
//         glutPostRedisplay();
//         break;
    case 'm':
    case 'M':
    	viewState=0;
    	break;
    case 'a':
    case 'A':
    	viewState=1;
    	break;
    case 'S':
    case 's':
    	viewState=2;
    	break;
    case 'I':
    case 'i':
    	viewState=3;
    	break;
    case 'F':
    case 'f':
    	viewState=4;
    	break;
    
    case 27:  /*  Escape Key  */
        exit(0);
        break;
     default:
        break;
    }
}