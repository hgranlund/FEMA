OBJECTS = FEMTypes.o FEMUtility.o FEMMath.o  FEMMethods.o FEM.o 

#FFLAGS = -g

FC = gfortran

.SUFFIXES: .f90 .o
.f90.o:
	$(FC) -c $<

all: test

# Husk at det skal være "tab" foran $(FC)
test: ${OBJECTS}
	$(FC) ${OBJECTS} -o FEM -lm

clean:
	rm FEM *.o *.mod *.Mod 

veryclean: 
	rm FEM *.o *.mod *.Mod *~ *.orig
