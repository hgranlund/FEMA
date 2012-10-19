
module FemMethods
  use FEMTypes
  implicit none

contains

  !###############################
  !LS LocalStiffnesMatrix er den genererte lokale stivhetsmatrisen
  !Elm er elementet i fokus
  !###############################
  subroutine LocalStiffness(LS, Elm)
    real , intent(out) :: LS(:,:)
    type (element), intent(in):: Elm

    real :: EI,EA,L,t1,t2,t3,t4
    character (len=21) :: Tag = '##### LocalStiffness:'

    L=Elm%L
    t1 = Elm%EA /L
    t2 = (12*Elm%EI)/L**3
    t3 = (6*Elm%EI)/L**2
    t4= (2*Elm%EI)/L
    if (pr_switch > 7)then
       print *,''
       print * , Tag
       print *, "t1:", t1, "t2:", t2, "t3:", t3, "t4:", t4
    end if
    LS(1,1)=t1
    LS(2,2)=t2
    LS(3,3)=2*t4
    LS(4,4)=t1  
    LS(5,5)=t2
    LS(6,6)=2*t4
    LS(4,1)=-t1
    LS(1,4)=-t1
    LS(2,5)=-t2
    LS(5,2)=-t2
    LS(5,3)=t3
    Ls(3,5)=t3
    LS(2,3)=-t3
    LS(3,2)=-t3
    LS(6,3)=2*t1
    LS(3,6)=2*t1
    LS(2,6)=-t3 
    LS(6,2)=-t3
    LS(5,6)=t3
    LS(6,5)=t3
  end subroutine LocalStiffness


  !###############################
  !En prosedyre som genererer  den globale stivhetsmatrisen
  !###############################
  subroutine GlobalStiffness(GlobalStiffnessMatrix, Elms,numberOfElm,errorFlag)
    integer ,intent(in)::numberOfElm
    integer ,intent(inout) :: errorFlag
    real , intent(out) :: GlobalStiffnessMatrix(:,:)
    type (element), intent(in):: Elms(:)


    integer :: i ,j, k
    real :: LocalStiffnessMatrix(6,6)
    type (element) :: elm
    integer :: GMC(6) ! GlobalMatrixConverter konverterer index  fra local til global stivhetsmatrise
    character (Len=23) :: Tag = '##### GlobalStiffness: '

    do i = 1, numberOfElm, 1
       elm = Elms(i)
       call LocalStiffness(LocalStiffnessMatrix,elm)
       do j=1,DOF,1
          GMC(j)=((elm%node1-1)*DOF) +j
          GMC(j+3)=((elm%node2-1)*DOF) +j
       end do
       if (pr_switch > 6)then
          print * ,''
          print *, Tag
          print *, 'Jobber på element ...: ', elm
          print *, 'GlobalMatrixConverter... : ' , GMC
          
       end if

       do j=1,6,1
          do k =1,6,1
             GlobalStiffnessMatrix(GMC(k),GMC(j))=GlobalStiffnessMatrix(GMC(k),GMC(j))+LocalStiffnessMatrix(k,j)
          end do
       end do
    end do
  end subroutine GlobalStiffness




end module FemMethods
