# FEMA


Simple Finite  Analysis for calculating loads in frame structures.


## Beskrivelse
Programmer kalkulerer krefter og forskyvninger på en bjelke kontruksjon. Hver bjelke har 6 grader av frihet. 

##Innstalasjon




## input.dat
Inputfilen skal inneholde all nødvendig data for å gjøre en FEA. Inputfilen er bygget opp på denne måten:

* AntallNoder, AntallElementer, AntallKrefter
* Liste med Noder med format: 

	x-verdi, y-verdi, frihet i x-retning, frihet i y-retning, rotasjonsfrihet
	
* Liste med Elementer med format:

	E-modul,Areal, I, node1, node2
	
* Liste med krefter, med format:
	
	NodeNummer, Hvilken frihetsgrad? (x=1,y=2,r=3), Value 
	
	
