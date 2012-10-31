module FEMUtility

	implicit none

	   ! pr_switch brukes til å bestemme hva som skal printes ut. 

    integer , parameter :: pr_switch=5

	contains 


   Subroutine PrintMatrix(A,l,b)
    real, intent(inout) :: A(:,:)
    integer , intent(in)::l,b
    integer i,j

    print * , ' #######################################'
    do i=1,l
       print *,(A(i,j), j=1,b)
    end do
    print *, '####################################### '
    print *,''
  end Subroutine PrintMatrix

end module FEMUtility
