
  module FEMTypes
  !###############################
  ! FEMTypes inneholder data typer brukt i FEM
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  use FEMUtility
    implicit none
    integer , parameter ::  DOF=3 !     ! DOF= Degrace of freedom. Maksimalt antall frihaetsgrader per node

    type  element
       real :: e,a,i,l, cosT , sinT, ForceVector(DOF*2), Displacement(DOF*2)
       integer :: node1, node2 
    end type element

    type  node 
       real :: x,y
       integer :: GDOF(DOF)
    end type node
     
    type  load
      integer :: nodeNr, DOF
       real :: value
    end type load

  end module FEMTypes
