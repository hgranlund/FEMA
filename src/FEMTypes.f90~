
  module FEMTypes
    implicit none
    
    ! pr_switch brukes til å bestemme hva som skal printes ut. 
    ! DOF= Degrace of freedom/node

    integer , parameter :: pr_switch=10, DOF=3

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
