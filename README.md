# FEMA


Simple Finite  Analysis for calculating loads in frame structures.


## input.dat
Inputfilen skal inneholde all nødvendig data for å gjøre en FEA. Inputfilen er bygget opp på denne måten:

* AntallNoder, AntallElementer, AntallKrefter
* Liste med Noder med format: 

	x-verdi, y-verdi, frihet i x-retning, frihet i y-retning, rotasjonsfrihet
	
* Liste med Elementer med format:

	E-modul*Areal, E-modul*I, Lengde, node1, node2
	
* Liste med krefter, med format:
	
	NodeNummer, Rotasjonen, i radianer, til vektoren (hvor nullpunktet er definert i positiv x-retning) ELLER en negativ verdi om det er moment, eks -1, verdien til kraften/momentet
	
	
