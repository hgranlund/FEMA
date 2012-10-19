program FEM
  use FEMMethods
  implicit none
  integer ::  errorFlag,numberOfNodes,numberOfElm,numberOfLoads
  type (element),ALLOCATABLE :: Elms(:)
  type (node), ALLOCATABLE :: Nodes(:)
  type (load), ALLOCATABLE :: Loads(:)
  real , ALLOCATABLE :: GlobalStiffnessMatrix(:,:)
  errorFlag=0
  call ReadInput()
  allocate (GlobalStiffnessMatrix(DOF*numberOfNodes, DOF*numberOfNodes))
  call GlobalStiffness(GlobalStiffnessMatrix,Elms,numberOfElm,errorFlag)
  call PrintMatrix(GlobalStiffnessMatrix,DOF*numberOfNodes, DOF*numberOfNodes)
  if (errorFlag > 0 ) then 
     print *,'Noe gikk feil '
  end if
contains



  Subroutine ReadInput()
    integer ::file_in,n
    file_in=10
    open(file_in, file="inputEasy.dat",status="old")
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
    integer :: file_out
    file_out = 11
    open(file_out,file="output.dat")
    !TODO: Skriv ut resultatene
  end Subroutine WriteOutput


  Subroutine PrintMatrix(A,l,b)
    real, intent(inout) :: A(:,:)
    integer i,j,b,l

    print * , '#######################################'
    do i=1,l
       print *,(A(i,j), j=1,b)
    end do
    print *, '####################################### '
  end Subroutine PrintMatrix
end program FEM

