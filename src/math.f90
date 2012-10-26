
! Gauss elimination with partial pivoting

module Math
 
  implicit none



contains

  subroutine GaussSolver(A,B,X,len, errorFlag) 
!=====================================================================================
    ! Task: Denne kjører en direkte gauss eliminasjon på matrisen A
    ! - input:
    ! - Ax=B
    ! output: 
    ! - A er den reduserte matrisen
    ! - X er løsningen
!===================================================================================

    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)

    integer i,j,k
    REAL tmp, lower_sigular_value, temp , pr_switch
    parameter (lower_sigular_value=1.0E-09)
   
    pr_switch =5
    if (errorFlag < 0) return
    
    do k=1, len-1 ! rad operasjoner, totalt len-1 operasjoner

       do i=k+1 ,len
          IF ( (ABS(A(I,k))-abs(A(k,k))).gt. 0) then   ! swapper rader slik at jeg får størst ...
             do j=k, len                               ! mulig pivot -- da slipper vi divisjon...
                 temp=A(k,j)                       ! med unødvendig små tall
                A(k,j)=A(i,j)
               A(i,j)=temp
             end do
             temp=B(k)
             B(k)=B(i)
             B(i)=temp
          end if
       end do

       if (pr_switch>5) then
          print *, 'Matrisen etter swap, iterasjon...: ' , k 
          call PrintMatrix(A,len,len)
       end if

       ! Tester om matrisen er singulær
       if (abs(A(k,k))< lower_sigular_value) THEN
          print *, 'Matrisen er singulær'
          errorFlag = -5
          return
       end if
       ! utfører radoperasjoner
       do  i= k+1, len
          if (a(i,i) .NE. 0) then
             do j= len, k, -1                     
                !             a(i,j) = a(i,j)* a(k,k) / a(i,k) - a(k,j)
                A(i,j) =  A(i,j)- A(i,k)/A(k,k)*A(k,j)
             end do
             B(i)=B(i)- A(i,k)/A(k,k)*B(k)
          else
             print *,' Det finnes ingen unik løsning '
             errorFlag=5
          end if
       end do
       if (pr_switch>5)then 
          print *, 'readoberasjon nummber ......: ' , k
          call PrintMatrix(A,len,len)
       end if
    end do


    ! tilbake substitusjon
    X(len)=A(len,len+1)/A(len,len)
    if (abs(A(k,k)) == 0) THEN
       print *, 'Matrisen har ikke en unik løsing'
       errorFlag = 5
    end if

    do k =len-1 , 1, -1
       tmp = 0.0d0
       do j = k+1, len
          tmp = tmp + A(k,j)*X(j)
       end do
       X(k)=(B(k)-tmp)/A(k,k)
    end do
    if (pr_switch >2)then
       print *, 'Matrisen etter gauss eliminisjon:'
       call PrintMatrix(A,len,len)
       print *, 'b matrix: ', B
    end if
  end subroutine GaussSolver

  real  function  AngelFromPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy
    dx=x2-x1
    dy=y2-y1
    AngelFromPoints = atan(dy/dx)
  end function AngelFromPoints
    
   Subroutine PrintMatrix(A,l,b)
    real, intent(inout) :: A(:,:)
    integer , intent(in)::l,b
    integer i,j

    print * , ' #######################################'
    do i=1,l
       print *,(A(i,j)/66670, j=1,b)
    end do
    print *, '####################################### '
    print *,''
  end Subroutine PrintMatrix


end module Math
