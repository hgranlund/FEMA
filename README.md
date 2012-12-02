# FEMA





## Beskrivelse
Simple Finite element Method for calculating loads in frame structures. It uses Fortran to do the calculations and c to visualusere the result.


## Installation
To download the project use:

	git clone https://github.com/hgranlund/FEMA.git
	
To build use the commands:

	cd src/;
	make;


## Usage
To run the program you can use 2 commands:

The first use a standard input file located in inputfiles/input.dat, the second takes a input file as input:

	./run
	
Or

	./runWithInput < inputFiles/input3.dat


## The Input file
The input file contains a set of values described in below.

	number of nodes,  number of elements, noumber of loads
	
List of nodes:


	x-value, y-value, DOF in x,y, rotation (0 or 1)
 List of elements:


	E-modul,Areal, Inertia, node1, node2
	
List of loads:

	
	Node number, WhatDegreeOfFreedom (x=1,y=2,r=3), Value 

	
	
