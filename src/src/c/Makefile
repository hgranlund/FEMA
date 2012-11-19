OBJECTS = main.o fem_graphics.o fem_draw.o fem_keyboard.o fem_file_reader.o fem_math.o

CFLAGS = -lglut -lGLU -lGL -lm

CC = gcc

all: FEMVis

# Husk at det skal være "tab" foran $(CC)
FEMVis: ${OBJECTS}
		$(CC) ${OBJECTS} -o FEMVis ${CFLAGS} 

clean:
		rm  -f FEMVis ${OBJECTS} *.o 

veryclean:
		rm  -f FEMVis ${OBJECTS} *~ *.orig *.o
