  
  module FemMethods
    use FEMTypes

    implicit none

  contains
    
    subroutine LocalStiffMatrix(LS, Elm)
      real , intent(out) :: LS(:,:)
      type (element), intent(in):: Elm

      real :: EI,EA,L,t1,t2,t3,t4
      t1 = Elm%EA / L
      t2 = (12*Elm%EI)/L**3
      t3 = (6*Elm%EI)/L**2
      t4= (2*Elm%EI)/L
      LS(1,1)=t1
      LS(2,2)=t2
      LS(3,3)=2*t4
      LS(4,4)=t1  
      LS(5,5)=t3
      LS(6,6)=2*t4
      LS(4,1)=-t1
      LS(1,4)=-t1
      LS(2,5)=-t2
      LS(5,2)=-t2
      LS(5,3)=t3
      Ls(3,5)=t3
      LS(2,3)=-t3
      LS(3,2)=-t3
      LS(6,3)=t1
      LS(3,6)=t1
      LS(2,6)=-t3 
      LS(6,2)=-t3
      LS(5,6)=t3
      LS(6,5)=t3
    end subroutine LocalStiffMatrix
  end module FemMethods