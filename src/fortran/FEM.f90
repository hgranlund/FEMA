!###############################
!FEM is the main program. It used the FEMMethods module to implement the Finite element method.
!  
!Some notation: 
! G = Global, L= Local, DOF = Degrees Of Freedom, Elm = Element,
! S=Stifness, FEM = FiniteElementMethod
!
!
! Author: Simen Haugerud Granlund
! Date modified: 29/11/12
!###############################



program FEM
  use FEMMethods
  implicit none
  integer ::  errorFlag,status,i
  real , ALLOCATABLE :: DisplacementVector(:)
  type (element),ALLOCATABLE :: Elms(:)
  type (node), ALLOCATABLE :: Nodes(:)
  type (load), ALLOCATABLE :: Loads(:)



  errorFlag=0

  call ReadInput() 
  allocate (DisplacementVector(totalDegreesOfFreedom(Nodes)), stat=errorFlag)
  IF (errorFlag .NE. 0)then
     print *, "***Not Enough Memory*** when allocating Displacement "
     return
  end IF
  
  !Start the FEM
  call DoFEM(DisplacementVector,Elms,Nodes,Loads, Errorflag)
  if (errorFlag .NE. 0 ) then 
     print *,'Errorflag = ' ,Errorflag
     stop
  end if
   call WriteOutput()

contains
  Subroutine ReadInput()
    integer ::n, numberOfNodes, numberOfElm,numberOfLoads, file_in
    type (element) :: elm


    file_in =5

    read(file_in,*,iostat=errorFlag) numberOfNodes, numberOfElm, numberOfLoads
    allocate (Nodes(numberOfNodes))
    allocate (Elms(numberOfElm))
    allocate (Loads(numberOfLoads))
    read (file_in,*,iostat=errorFlag) (Nodes(n), n=1,numberOfNodes)
    read (file_in,*, iostat=errorFlag) (Elms(n)%e,Elms(n)%a,Elms(n)%i,Elms(n)%node1,Elms(n)%node2,n=1,numberOfElm)
    read (file_in,*, iostat=errorFlag) (Loads(n),n=1,numberOfLoads)
    if ( errorFlag /= 0 ) stop "Something wrong with input file"


    !Testing the input file data
    do n=1,numberOfLoads
      if (Loads(n)%nodeNr>numberOfNodes)then 
        print * , 'Load is pointing to a nonexistent node'
        errorFlag = -4
        return;
      end if
      if ( Loads(n)%DOF >3) then
        print *, 'FEM does not support more then 3 DOF'
        errorFlag=-4 
      end if
    end do

    do n=1,numberOfElm
      if (Elms(n)%node1>numberOfNodes)then 
        print * , 'Load is pointing to a nonexistent node'
        errorFlag = -4
        return;
         end if
      if ( Elms(n)%node2>numberOfNodes ) then
        print * , 'Load is pointing to a nonexistent node'
        errorFlag = -4
        return;
      end if

    end do

  


    if (pr_switch >= 5 ) then 
       print *,'ReadInput:'
       print *, 'Antall noder........: ', numberOfNodes 
       print *, 'Antall elementer....: ', numberOfElm 
       print *, 'Antall krefter......: ', numberOfLoads
    end if

  end Subroutine ReadInput

  Subroutine WriteOutput()
    integer ::n, numberOfElm, numberOfLoads, file_out

      file_out=11
      open(unit=file_out, file='FEMOutput.dat', iostat=errorFlag, status="unknown", action="write")
      if ( errorFlag /= 0 ) stop "Error opening file_out"

    numberOfLoads=size(Loads)
    numberOfElm = size(Elms)
    write(file_out,*, iostat=errorFlag), numberOfElm, numberOfLoads, BiggestNodeCoordinate(Nodes)

    do n=1,numberOfElm
      write (file_out,fmt="( F15.4 F15.4 F15.4 F15.4)",iostat=errorFlag), Nodes(Elms(n)%node1)%x,&
      & Nodes(Elms(n)%node1)%y,Nodes(Elms(n)%node2)%x,Nodes(Elms(n)%node2)%y
    end do
    do n=1,numberOfElm
      write (file_out,fmt="( F15.4 F15.4 F15.4 F15.4 F15.4 F15.4 )",iostat=errorFlag), Elms(n)%ForceVector
    end do
    do n=1,numberOfElm
      write (file_out,fmt="( F15.4 F15.4 F15.4 F15.4 F15.4 F15.4 )",iostat=errorFlag), Elms(n)%Displacement
    end do
    do n=1,numberOfLoads
      write (file_out,fmt="( I15 F15.4 F15.4 F15.4)",iostat=errorFlag), Loads(n)%DOF, Nodes(Loads(n)%nodeNr)%x, &
      & Nodes(Loads(n)%nodeNr)%y, Loads(n)%value
    end do
    
    if ( errorFlag /= 0 ) stop "Cant make FEMOutput.dat" 




  end Subroutine WriteOutput

end program FEM
