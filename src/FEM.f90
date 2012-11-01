program FEM
  use FEMMethods
  implicit none
  integer ::  errorFlag,status,i,file_in,file_out
  real , ALLOCATABLE :: DisplacementVector(:)
  type (element),ALLOCATABLE :: Elms(:)
  type (node), ALLOCATABLE :: Nodes(:)
  type (load), ALLOCATABLE :: Loads(:)


  file_in=5
  file_out=6
  errorFlag=0

!   file_in=10
!   file_out=11
!   open(unit=file_in, file='input.dat', iostat=errorFlag, status="old", action="read")
!   if ( ios /= 0 ) stop "Error opening file_in"
!   open(unit=file_out, file='output.dat', iostat=errorFlag, status="new", action="write")
!   if ( ios /= 0 ) stop "Error opening file_out"
  


  call ReadInput() 
  allocate (DisplacementVector(totalDegrees(Nodes)), stat=errorFlag)
  IF (errorFlag .NE. 0)then
     print *, "***Not Enough Memory*** when allocating Displacement "
     return
  end IF
  
  call DoFEA(DisplacementVector,Elms,Nodes,Loads, Errorflag)
  if (errorFlag .NE. 0 ) then 
     print *,'Errorflag = ' ,Errorflag
     stop
  end if
!   call WriteOutput()

contains
  Subroutine ReadInput()
    integer ::n, numberOfNodes, numberOfElm,numberOfLoads
    type (element) :: elm

    read(file_in,*,iostat=errorFlag) numberOfNodes, numberOfElm, numberOfLoads
    allocate (Nodes(numberOfNodes))
    allocate (Elms(numberOfElm))
    allocate (Loads(numberOfLoads))
    read (file_in,*,iostat=errorFlag) (Nodes(n), n=1,numberOfNodes)
    read (file_in,*, iostat=errorFlag) (Elms(n)%e,Elms(n)%a,Elms(n)%i,Elms(n)%node1,Elms(n)%node2,n=1,numberOfElm)
    read (file_in,*, iostat=errorFlag) (Loads(n),n=1,numberOfLoads)
    if ( errorFlag /= 0 ) stop "Read error in file file_in"

    if (pr_switch >= 5 ) then 
       print *,'ReadInput:'
       print *, 'Antall noder........: ', numberOfNodes 
       print *, 'Antall elementer....: ', numberOfElm 
       print *, 'Antall krefter......: ', numberOfLoads
    end if

  end Subroutine ReadInput

  Subroutine WriteOutput()
    integer ::n

    write(file_out, *, iostat=errorFlag) DisplacementVector
    write (file_out,*,iostat=errorFlag), '==============================================================='
    write(file_out,*, iostat=errorFlag) 'Elementer: [E-modul, Areal, Inertia, Lengde, Cos(theta), &
    &Sin(Theta), KraftVektor, Node1, Node2'
    
    do n=1, ubound(Elms,1)
      write(file_out,*, iostat=errorFlag) Elms(n)
      write(file_out,*, iostat=errorFlag) ''
    end do
       if ( errorFlag /= 0 ) stop "Write error in file file_out"

  end Subroutine WriteOutput

end program FEM
