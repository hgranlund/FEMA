
  module FEMTypes
    implicit none
    
    ! NON=Nomber Of Nodes/ Beam
    ! DOF= Degrace of freedom/node
    integer , parameter :: NON=2, DOF=3

    type :: element
       real :: EA,EI,Len
       integer :: Joints(NON)
    end type element

    type :: joint 
       real :: X,Y
       integer :: GDOF(DOF)
    end type joint
  end module FEMTypes
