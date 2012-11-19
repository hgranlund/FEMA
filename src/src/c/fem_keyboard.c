#include "fem_keyboard.h"

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
    case 27:  /*  Escape Key  */
        exit(0);
        break;
        default:
        break;
    }
}