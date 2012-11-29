  !###############################
  ! FEMTypes inneholder data typer brukt i FEM. 
  ! What the values stands for is written in readme.md
  !
  ! Author: Simen Haugerud Granlund
  ! Date modified: 29/11/12
  !###############################

  module FEMTypes

  use FEMUtility
    implicit none
    integer , parameter ::  DOF=3 !     ! DOF= Degrace of freedom. Maximum number of DOF in each node

    type  element
       real :: e,a,i,l, cosT , sinT, ForceVector(DOF*2), Displacement(DOF*2)
       integer :: node1, node2 
    end type element

    type  node 
       real :: x,y
       integer :: GDOF(DOF)              ! Describes that DOF the node has
    end type node
     
    type  load
      integer :: nodeNr, DOF
       real :: value
    end type load

  end module FEMTypes
