OBJECTS = FEMVis.o fem_draw.o fem_keyboard.o fem_file_reader.o draw_util.o fem_math.o 

CFLAGS = -lglut -lGLU -lGL -lm

CC = gcc

all: FEMVis


FEMVis: ${OBJECTS}
		$(CC) ${OBJECTS} -o FEMVis ${CFLAGS} 

clean:
		rm  -f FEMVis ${OBJECTS} *.o 

veryclean:
		rm  -f FEMVis ${OBJECTS} *~ *.orig *.o
