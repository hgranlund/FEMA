program FEM
  use FEMMethods
  implicit none
  integer ::  errorFlag, pr_switch, file_in, file_out, EI,EA,L
  integer :: Joints(2)
  REAL, ALLOCATABLE, DIMENSION(:,:) :: A
  REAL,  ALLOCATABLE, DIMENSION(:) :: B

  type (element) :: elm
  errorFlag=0
  pr_switch=10; ! Denne brukes til å bestemme hvor mye som skal printes

  file_in=10
  file_out=11
  open(file_in, file="input.dat",status="old")
  open(file_out,file="output.dat")
  allocate (A(6,6))
  EI=2000
  EA=1000
  L=2
  Joints(1)=1
  Joints(2)=2
  elm%EA=EA
  elm%EI=EI
  elm%L=L
  elm%Joints=Joints
  print *, elm%EA
  call  LocalStiffMatrix(A,elm)
  call  PrintMatrix(A,6,6)
  if (errorFlag >= 0 ) then 
     print *,'Marisen etter gauss........: '
  end if


contains 
  Subroutine PrintMatrix(A,l,b)
    real, intent(inout) :: A(:,:)
    integer i,j,b,l
    do i=1,l
       print *,(A(i,j), j=1,b)
    end do
    print *, ' '
  end Subroutine PrintMatrix
end program FEM

