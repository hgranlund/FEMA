
  module FEMTypes
  use FEMUtility
    implicit none
    integer , parameter ::  DOF=3

    type :: element
       real :: e,a,i,l
       integer :: node1, node2
    end type element

    type :: node 
       real :: x,y
       
       integer :: GDOF(DOF)
    end type node
     
    type :: load
       integer :: nodeNr, dof
       real :: value
    end type load

  end module FEMTypes
