

module FEMMath
  implicit none

contains



  ! Gauss elimination with partial pivoting

  subroutine GaussSolver(A,B,X,len,errorFlag)
    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)

    if (errorFlag < 0) return

    call GaussElimination(A,B,X,len,errorFlag)
    call BackwardSubstitution(A,B,X,len,errorFlag)
  end subroutine GaussSolver


  subroutine GaussElimination(A,B,X,len,errorFlag)
    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)
    

    integer i,j,k
    real  :: akk

     do k=1, len-1
       do i=k+1 ,len
          IF ( (ABS(A(I,k))-abs(A(k,k))).gt. 0) then   
                call swapAB(A,B,k,i)
          end if
       end do

       ! Tester om matrisen er singulær
       if (abs(A(k,k))< epsilon(A(k,k))) THEN
          print *, 'Matrisen er singulær'
          errorFlag = -5
          return
       end if

       ! utfører radoperasjoner
       do  i= k+1, len
          akk=A(k,k)
          do j= k+1, len, 1                     
             A(i,j) =  A(i,j)- A(k,j)*(A(i,k)/akk)
          end do
          B(i)=B(i)- A(i,k)*B(k)/A(k,k)
          A(i,k) =0 
       end do
    end do
  end subroutine GaussElimination


  subroutine BackwardSubstitution(A,B,X,len ,errorFlag)
    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)

    integer :: j,k
    real :: tmp

    do k =len , 1, -1
       tmp = 0.0d0
       do j = k+1, len
          tmp = tmp + A(k,j)*X(j)
       end do
       X(k)=(B(k)-tmp)/A(k,k)
       if (abs(A(k,k)) == 0) THEN
          errorFlag = 5
          x(k)=1 
          print *, ''
          print *, '####There is no unik solution! Set x',k,'equals 1'
       end if
    end do
  end subroutine BackwardSubstitution

  subroutine GaussSeidel(A,B,X,len,errorFlag)
    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)
    
    integer i,j,k, iteration
    real  :: tempError, maxError, presition, XOld

    maxError=0;
    iteration=1E6
    presition=1E-5
    XOld=1
    X(:)=1
     do k=1, iteration
      maxError=0  
       do i=1, len
          XOld=X(i)
          X(i)=B(i)/A(i,i)
          do j= 1, len
              if (j .EQ. i) cycle    
              X(i)= X(i) - ((A(i,j) * X(j)) / A(i,i) )              
          end do
          tempError = abs(X(i)-XOld)
          if (tempError .GT. maxError) maxError=tempError
       end do
       if (maxError .LT. presition)then
        print *,k
        return 
      end if
    end do
    print * , 'GaussSeidel fant ikke løsning'
    errorFlag =4
  end subroutine GaussSeidel


  real  function  AngelFromPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy

    dx=x2-x1
    dy=y2-y1
    AngelFromPoints = atan(dy/dx)
  end function AngelFromPoints

  real  function  LengthBetweenPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy

    dx=x2-x1
    dy=y2-y1
    LengthBetweenPoints = sqrt(dx**2+dy**2)
  end function LengthBetweenPoints


  function RotationMatrix(cosT,sinT)
    real, intent(in) :: cosT, sinT 

    real :: RotationMatrix(6,6)

    call NullifyMatrix(RotationMatrix)

    RotationMatrix(3,3)=1
    RotationMatrix(6,6)=1
    RotationMatrix(1,1)=cosT
    RotationMatrix(2,2)=cosT
    RotationMatrix(4,4)=cosT
    RotationMatrix(5,5)=cosT
    RotationMatrix(1,2)=-sinT
    RotationMatrix(2,1)=sinT
    RotationMatrix(4,5)=-sinT
    RotationMatrix(5,4)=sinT

  end function RotationMatrix


  subroutine SwapRow(A,r1,r2)
    real, intent(inout) :: A(:,:)
    integer, intent(in) :: r1,r2

    real :: swap(size(A, 1))

    swap=A(r1,:)
    A(r1,:)=A(r2,:)
    A(r1,:)=swap

  end subroutine swapRow


  ! swapRow bytter rad r1 med r2 i matrisen A og vektoren B

  subroutine swapAB(A,B,r1,r2)
    real, intent(inout) :: A(:,:), B(:)
    integer, intent(in) :: r1,r2

    real :: swapA(size(A, 1)), swapB

    call swapRow(A,r1,r2)
    swapB=B(r1)
    B(r1)=B(r2)
    B(r1)=swapB
  end subroutine swapAB

  subroutine NullifyMatrix(Matrix)
    real, intent(inout) :: Matrix(:,:) 

    integer i,j

    forall (i=lbound(Matrix,1):ubound(Matrix,1), j=lbound(Matrix,2):ubound(Matrix,2)) Matrix(i,j)=0
    
  end subroutine NullifyMatrix

end module FEMMath
