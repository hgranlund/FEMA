
  module FEMTypes
    implicit none
    
    ! NON=Nomber Of Nodes/ Beam
    ! DOF= Degrace of freedom/node
    integer , parameter :: NON=2, DOF=3

    type :: element
       real :: EA,EI,L
       integer :: node1, node2
    end type element

    type :: node 
       real :: X,Y
       integer :: GDOF(DOF)
    end type node
     
    type :: load
       !TODO: bestemme verdier.
    end type load

  end module FEMTypes
