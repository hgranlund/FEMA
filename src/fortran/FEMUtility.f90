  !##############################
  !FEMUtility contains utility functions for the FEMModule
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################



module FEMUtility
  use FEMTypes
  implicit none

	contains 


  subroutine LocalElementStiffness(LS, Elm)
    real , intent(out) :: LS(:,:)
    type (element), intent(in):: Elm

    integer::i,j
    real :: ei,ea,l,t1,t2,t3,t4

    call NullifyRealMatrix(LS)

    ea=elm%e*Elm%a
    ei=elm%e*elm%i
    l=elm%l
    t1 = ea /l
    t2 = (12*ei)/l**3
    t3 = (6*ei)/l**2
    t4= (2*ei)/l
    LS(1,1)=t1
    LS(4,1)=-t1
    LS(2,2)=t2
    LS(3,2)=-t3
    LS(5,2)=-t2
    LS(6,2)=-t3
    LS(2,3)=-t3
    LS(3,3)=2*t4
    LS(5,3)=t3
    LS(6,3)=t4
    LS(1,4)=-t1
    LS(4,4)=t1  
    LS(2,5)=-t2
    Ls(3,5)=t3
    LS(5,5)=t2
    LS(6,5)=t3
    LS(2,6)=-t3 
    LS(3,6)=t4
    LS(5,6)=t3
    LS(6,6)=2*t4

    if (pr_switch > 7)then
       print *,''
       print * , '##### LocalElementStiffness:'
       print *, "t1:", t1, "t2:", t2, "t3:", t3, "t4:", t4
       call PrintMatrix(LS)
    end if
  end subroutine LocalElementStiffness




  subroutine GlobalElementStiffness(LS, elm)
    real , intent(out) :: LS(:,:)
    type (element), intent(in):: elm

    integer::i,j
    real :: e,a,inertia,l,t1,t2,t3,c,s

    s=elm%sinT
    c=elm%cosT
    e=elm%e
    a=elm%a
    inertia=elm%i
    l=elm%l
    t1 = (12 *inertia) /l**2
    t2 = (6*inertia)/l
    t3 = e/l


    LS(1,1)=t3*((a*c**2)+(t1*s**2))
    LS(2,1)=t3*(a-t1)*c*s
    LS(3,1)=t3*(t2*s)
    LS(4,1)=-LS(1,1)
    LS(5,1)=t3*(((12/l**2)-a)*c*s)
    LS(6,1)=LS(3,1)

    LS(1,2)=LS(2,1)
    LS(2,2)=t3*((a*s**2)+(t1*c**2))
    LS(3,2)=-t3* (6*inertia*c/l)
    LS(4,2)=LS(5,1)
    LS(5,2)=-LS(2,2)
    LS(6,2)=LS(3,2)

    LS(1,3)=LS(3,1)
    LS(2,3)=LS(3,2)
    LS(3,3)=t3*4*inertia
    LS(4,3)=-LS(3,1)
    LS(5,3)=-LS(3,2)
    LS(6,3)=t3*2*inertia


    LS(1,4)=LS(4,1)
    LS(2,4)=LS(4,2)
    LS(3,4)=LS(4,3)
    LS(4,4)=LS(1,1)
    LS(5,4)=LS(2,1)
    LS(6,4)=-LS(6,1)

    LS(1,5)=LS(5,1)
    LS(2,5)=LS(5,2)
    LS(3,5)=LS(5,3)
    LS(4,5)=LS(5,4)
    LS(5,5)=LS(2,2)
    LS(6,5)=LS(5,3)

    LS(1,6)=LS(6,1)
    LS(2,6)=LS(6,2)
    LS(3,6)=LS(6,3)
    LS(4,6)=LS(6,4)
    LS(5,6)=LS(6,5)
    LS(6,6)=LS(3,3)

    if ( pr_switch >8 ) then
       print *, ''
       print *, '###### LocalStiffnessMatrix \n'
       call PrintMatrix(LS)
    end if
  end subroutine GlobalElementStiffness


  subroutine PopulateLoads(LoadVector,Loads,GTRGConverter,errorflag)
    integer, intent(in) :: GTRGConverter(:),Errorflag
    type (load), intent(in) :: Loads(:)
    real , intent(out):: LoadVector(:)

    integer :: i, globalIndex

    if (Errorflag .LT. 0) return

    do i=1,ubound(Loads, 1)
       globalIndex=(Loads(i)%nodeNr-1)*3+Loads(i)%DOF
       globalIndex= GTRGConverter(globalIndex)
       if (globalIndex==0) cycle
       LoadVector(globalIndex)=Loads(i)%value
    end do

    if (pr_switch>5)then
       print *, '##### PopulateLoads:'
       print *, 'LoadVector', LoadVector
    end if

  end subroutine PopulateLoads


  !GlobalToReducedGlobalStiffnessMatrixConverter converts the indexes 
  !from the global stiffnesmatrix to the redused global stiffnessmatrix
  Subroutine GToReducedGStiffnessMatrixConverter(GTRGConverter,Nodes)
    type (node), intent(in):: Nodes(:)

    integer ,intent(out) :: GTRGConverter(size(Nodes)*3)

    integer ::i,j,globalIndex,redusedIndex

    globalIndex=1
    redusedIndex=1
    do i=1,ubound(Nodes, dim=1)
       do j=1,3
          if (Nodes(i)%GDOF(j) .NE. 0)then
             GTRGConverter(globalIndex)=redusedIndex
             redusedIndex=redusedIndex+1
          else
             GTRGConverter(globalIndex)=0
          end if
          globalIndex=globalIndex+1
       end do
    end do

    if(pr_switch>7)then
       print *,''
       print *, 'Global til redusert globalestivhetsmatrise konvertor:'
       print *, GTRGConverter
    end if
  end Subroutine GToReducedGStiffnessMatrixConverter




   Subroutine PrintMatrix(A)
    real, intent(inout) :: A(:,:)

    integer i,j

    print * , ' #######################################'
    do i=lbound(A,1), ubound(A,1)
       print *,(A(i,j)/66670, j=lbound(A,2), ubound(A,2))
    end do
    print *, '####################################### '
    print *,''
  end Subroutine PrintMatrix


  subroutine NullifyRealMatrix(Matrix)
    real, intent(inout) :: Matrix(:,:) 

    integer i,j

    forall (i=lbound(Matrix,1):ubound(Matrix,1), j=lbound(Matrix,2):ubound(Matrix,2)) Matrix(i,j)=0

  end subroutine NullifyRealMatrix


  subroutine NullifyRealVector(Vector)
    real, intent(inout) :: Vector(:) 

    integer i

    forall (i=lbound(Vector,1):ubound(Vector,1)) Vector(i)=0
    
  end subroutine NullifyRealVector

  subroutine TestInfAndNaN(Vector, errorflag)
    real, intent(inout) :: Vector(:) 
    integer, intent(inout)  :: errorflag

    integer i
    do i=lbound(Vector,1), ubound(Vector,1)
      if ((Vector(i) - 1000.0) .eq. Vector(i)) then
        errorflag = -3
        return
      end if
    end do
  end subroutine TestInfAndNaN

 end module FEMUtility
