

module FEMMath
  use FEMUtility
  implicit none

contains


  !###############################
  ! Gauss elimination with partial pivoting
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  subroutine GaussSolver(A,B,X,len,errorFlag)
    integer, intent(in) :: len
    integer, intent(inout) :: errorFlag
    real, intent(inout)  :: A(len,len), B(len)
    REAL, intent(out) :: X(len)
    integer i,j,k
    REAL  :: akk


    if (errorFlag < 0) return

    do k=1, len-1
       do i=k+1 ,len
          IF ( (ABS(A(I,k))-abs(A(k,k))).gt. 0) then   
                call swapAB(A,B,k,i)
          end if
       end do

       if (pr_switch>9) then
          print * ,''
          print *, 'Matrisen etter swap, iterasjon...: ' , k 
          call PrintMatrix(A)
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
    end do

    call BackwardSubstitution(A,B,X,len,errorFlag)

    if (pr_switch >5)then
       print *, ''
       print *, 'Matrisen A etter gauss eliminisjon:'
       call PrintMatrix(A)
       print *, 'B matrix: ', B
    end if
  end subroutine GaussSolver


  !###############################
  ! Tilbake substitusjon
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

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
          print *, ''
          print *, 'Matrisen har ikke en unik løsing'
          errorFlag = 5
          x(k)=1 !om det ikke finnes en unik løsning setter jeg x = 1
       end if
    end do
  end subroutine BackwardSubstitution


  !###############################
  ! Funksjonen retunerer vinklen (rad) mellom x-aksen og linjen som er definert av punktene (x1,y1) og (x2,y2)
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  real  function  AngelFromPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy

    dx=x2-x1
    dy=y2-y1
    AngelFromPoints = atan(dy/dx)
  end function AngelFromPoints


  !###############################
  ! Funksjonen retunerer lengden til linjen som er definert av punktene (x1,y1) og (x2,y2)
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  real  function  LengthBetweenPoints(x1,y1,x2,y2)
    real, intent(in)::x1,y1,x2,y2

    real :: dx,dy

    dx=x2-x1
    dy=y2-y1
    LengthBetweenPoints = sqrt(dx**2+dy**2)
  end function LengthBetweenPoints

  !###############################
  ! Retunerer rotasjonsmatrisen med 6 frihetsgrader 
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  function RotationMatrix(cosT,sinT)
    real, intent(in) :: cosT, sinT 

    real :: RotationMatrix(6,6)

    call NullifyRealMatrix(RotationMatrix)

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



  !###############################
  ! swapRow bytter rad r1 med r2 i matrisen A og vector B
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################

  subroutine swapAB(A,B,r1,r2)
    real, intent(inout) :: A(:,:), B(:)
    integer, intent(in) :: r1,r2

    real :: swapA(size(A, 1)), swapB

    call swapRow(A,r1,r2)
    swapB=B(r1)
    B(r1)=B(r2)
    B(r1)=swapB
  end subroutine swapAB

end module FEMMath
