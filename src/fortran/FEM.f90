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
!   open(unit=file_in, file='inputE52S224.dat', iostat=errorFlag, status="old", action="read")
!   if ( errorFlag /= 0 ) stop "Error opening file_in"
!   open(unit=file_out, file='output.dat', iostat=errorFlag, status="unknown", action="write")
!   if ( errorFlag /= 0 ) stop "Error opening file_out"
  


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
   call WriteOutput()

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
    integer ::n, numberOfElm

    numberOfElm = size(Elms)
    write(file_out,*, iostat=errorFlag), numberOfElm
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
    
!     write (file_out,*,iostat=errorFlag), '==============================================================='
!     write (file_out,*,iostat=errorFlag), '==============================================================='
!     write(file_out,*, iostat=errorFlag) 'Forskyvningsvector'
!     write(file_out, *, iostat=errorFlag) DisplacementVector
!     write (file_out,*,iostat=errorFlag), ''
!     write (file_out,*,iostat=errorFlag), '==============================================================='
!     write(file_out,*, iostat=errorFlag) 'ElementNr:' 
!     write(file_out,*, iostat=errorFlag) '  Areal,          Lengde,         Cos(theta),       Node1,        Node2'
!     write(file_out,*, iostat=errorFlag) '  Fx1,            Fy1,            Mr1;               Fx2,          Fy2,           Mr2 '
!     write(file_out,*, iostat=errorFlag) ''
    

!     do n=1, ubound(Elms,1)
!       write(file_out,*, iostat=errorFlag) 'ElementNr:', n
!       write(file_out,*, iostat=errorFlag) Elms(n)%a,Elms(n)%l,Elms(n)%cosT,Elms(n)%node1,Elms(n)%node2
!       write(file_out,*, iostat=errorFlag) Elms(n)%ForceVector
!       write(file_out,*, iostat=errorFlag) ''

!     end do
    if ( errorFlag /= 0 ) stop "Write error in file file_out"

  end Subroutine WriteOutput

end program FEM
