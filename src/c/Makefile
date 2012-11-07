OBJECTS = bezier.o texobj.o

CFLAGS = -lglut -lGLU -lGL -lm

CC = gcc

all: FEMVis

# Husk at det skal være "tab" foran $(CC)
FEMVis: ${OBJECTS}
		$(CC) bezier.c -o bezier ${CFLAGS} 
		$(CC) texobj.c -o texobj ${CFLAGS} 

clean:
		rm  -f FEMVis ${OBJECTS} *.o 

veryclean:
		rm  -f FEMVis ${OBJECTS} *~ *.orig *.o
