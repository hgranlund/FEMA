
! Gauss elimination with partial pivoting

program Math
  implicit none


contains

  subroutine GaussSolver(A,len,B, errorFlag) 
!=====================================================================================
    ! Task: Denne kjører en direkte gauss eliminasjon på matrisen A
    ! - input:
    ! - A(len)(len+1) er hele matrisen
    ! - A(len+1)(len+1) er løsningsraden
    ! output: 
    ! - A er den reduserte matrisen
    ! - b er løsningen
!===================================================================================

    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len+1)      ! ANTAR LEN ER KORREKT
    REAL, intent(out) :: B(len)

    integer i,j,k, pr_switch
    REAL tmp, lower_sigular_value, swap_row(len+1)
    parameter (lower_sigular_value=1.0E-05)
    pr_switch=0
   

    if (errorFlag < 0) return

    do k=1, len-1 ! rad operasjoner, totalt len-1 operasjoner

       do i=k+1 ,len
          IF ( (ABS(A(I,k))-abs(A(k,k))).gt. 0) then   ! swapper rader slik at jeg får størst ...
             do j=k, len+1                               ! mulig pivot -- da slipper vi divisjon...
                 swap_row(j)=A(k,j)                       ! med unødvendig små tall
                A(k,j)=A(i,j)
               A(i,j)=swap_row(j)
             end do
          end if
       end do

       if (pr_switch>5) then
          print *, 'Matrisen etter swap, iterasjon...: ' , k 
          call PrintMatrix(A,len,len+1)
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
             do j= len+1, k, -1                     
                !             a(i,j) = a(i,j)* a(k,k) / a(i,k) - a(k,j)
                A(i,j) =  A(i,j)- A(i,k)/A(k,k)*A(k,j)
             end do
          else
             print *,' Det finnes ingen unik løsning '
             errorFlag=5
          end if
       end do
       if (pr_switch>5)then 
          print *, 'readoberasjon nummber ......: ' , k
          call PrintMatrix(A,len,len+1)
       end if
    end do


    ! tilbake substitusjon
    b(len)=A(len,len+1)/A(len,len)
    if (abs(A(k,k))< lower_sigular_value) THEN
       print *, 'Matrisen har ikke en unik løsing'
       errorFlag = 5
    end if

    do k =len-1 , 1, -1
       tmp = 0.0d0
       do j = k+1, len
          tmp = tmp + A(k,j)*b(j)
       end do
       b(k)=(A(k,len+1)-tmp)/A(k,k)
    end do
    if (pr_switch >2)then
       print *, 'Matrisen etter gauss eliminisjon:'
       call PrintMatrix(A,len,len+1)
    end if
    return
  end Subroutine GaussEli

  Subroutine PrintMatrix(A,l,b)
    real, intent(inout) :: A(:,:)
    integer i,j,b,l
    do i=1,l
       print *,(A(i,j), j=1,b)
    end do
    print *, ' '
  end Subroutine PrintMatrix
end Program Math



