
! Gauss elimination with partial pivoting

module Math
  use FEMUtility
  implicit none



contains

  subroutine GaussSolver(A,B,X,len,errorFlag)
  integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)
    integer i,j,k
    integer,parameter :: dp = selected_real_kind(15, 307)
    REAL  temp , akk, pr_switch

   
    pr_switch =0
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
          print *, B

       end if

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
       if (pr_switch>5)then 
          print *, 'readoberasjon nummber ......: ' , k
          call PrintMatrix(A,len,len)
          print *, B

       end if
    end do

    call BackwardSubstitution(A,B,X,len,errorFlag)
  end subroutine GaussSolver


    ! tilbake substitusjon
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
         print *, 'Matrisen har ikke en unik løsing'
         errorFlag = 5
         x(k)=1 !om det ikke finnes en unik løsning setter jeg x = 1
      end if
    end do
    if (5 >2)then
       print *, 'Matrisen etter gauss eliminisjon:'
       call PrintMatrix(A,len,len)
       print *, 'b matrix: ', B
    end if
  end subroutine BackwardSubstitution

  real  function  AngelFromPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy
    dx=x2-x1
    dy=y2-y1
    AngelFromPoints = atan(dy/dx)
  end function AngelFromPoints
    


Logical function closeToZeroBug(number)
  real, intent(in) :: number

  if ( abs(number) < epsilon(number)) then
    closeToZeroBug = .True.
  else 
    closeToZeroBug = .False.
  end if  
    
end function closeToZeroBug


end module Math
