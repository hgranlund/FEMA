module FEMUtility

	implicit none

	   ! pr_switch brukes til å bestemme hva som skal printes ut. 

    integer , parameter :: pr_switch=10

	contains 


  !###############################
  !Rutinen printer Matrisen A
  !###############################

   Subroutine PrintMatrix(A)
    real, intent(inout) :: A(:,:)

    integer i,j

    print * , ' #######################################'
    do i=lbound(A,1), ubound(A,1)
       print *,(A(i,j), j=lbound(A,2), ubound(A,2))
    end do
    print *, '####################################### '
    print *,''
  end Subroutine PrintMatrix



  !###############################
  !Prosedyren Setter alle elementene i Matrix til null
  !###############################

  subroutine NullifyRealMatrix(Matrix)
    real, intent(inout) :: Matrix(:,:) 

    integer i,j

    forall (i=lbound(Matrix,1):ubound(Matrix,1), j=lbound(Matrix,2):ubound(Matrix,2)) Matrix(i,j)=0
    
  end subroutine NullifyRealMatrix


    !###############################
  !Prosedyren Setter alle elementene i vector til null
  !###############################

  subroutine NullifyRealVector(Vector)
    real, intent(inout) :: Vector(:) 

    integer i

    forall (i=lbound(Vector,1):ubound(Vector,1)) Vector(i)=0
    
  end subroutine NullifyRealVector

end module FEMUtility
