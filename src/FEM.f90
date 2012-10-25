program FEM
  use FEMMethods
  implicit none
  integer ::  errorFlag,numberOfNodes,numberOfElm,numberOfLoads,numberOfTotalDegrees,status,i
  real , ALLOCATABLE :: DisplacementVector(:)
  type (element),ALLOCATABLE :: Elms(:)
  type (node), ALLOCATABLE :: Nodes(:)
  type (load), ALLOCATABLE :: Loads(:)

  errorFlag=0
  call ReadInput() 
  allocate (DisplacementVector(totalDegrees(Nodes,numberOfNodes)), stat=status)
  IF (status .NE. 0)then
     Errorflag =  status
     print *, "***Not Enough Memory*** when allocating Displacement "
     return
  end IF
  
  call CalcDisplacement(DisplacementVector,Elms,Nodes,Loads,numberOfElm,numberOfNodes,numberOfLoads, Errorflag)

  if (errorFlag .NE. 0 ) then 
     print *,'Noe gikk feil '
     print *,'Errorflag = ' ,Errorflag
     stop
  end if
  call WriteOutput
!  print * ,Displacement

contains
  Subroutine ReadInput()
    integer ::file_in,n
    file_in=10
    open(file_in, file="inputS219.dat",status="old")
    read(file_in,*) numberOfNodes, numberOfElm, numberOfLoads
    allocate (Nodes(numberOfNodes))
    allocate (Elms(numberOfElm))
    allocate (Loads(numberOfLoads))
    read (file_in,*) (Nodes(n), n=1,numberOfNodes)
    read (file_in,*) (Elms(n),n=1,numberOfElm)
    read (file_in,*) (Loads(n),n=1,numberOfLoads)
    if (pr_switch >= 5 ) then 
       print *,'ReadInput:'
       print *, 'Antall noder........: ', numberOfNodes 
       print *, 'Antall elementer....: ', numberOfElm 
       print *, 'Antall krefter......: ', numberOfLoads
    end if
  end Subroutine ReadInput

  Subroutine WriteOutput()
    integer ::file_out,n
    file_out=11
  !  open(file_out,file="output.dat")
    !TODO: Skriv ut resultatene
  end Subroutine WriteOutput

end program FEM
