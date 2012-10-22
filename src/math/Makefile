OBJECTS = gauss.o

#FFLAGS = -g

FC = gfortran

.SUFFIXES: .f90 .o
.f90.o:
	$(FC) -c $<

all: test

# Husk at det skal være "tab" foran $(FC)
test: ${OBJECTS}
	$(FC) ${OBJECTS} -o gauss -lm

clean:
	rm gauss *.o *.mod *.Mod ${OBJECTS}
