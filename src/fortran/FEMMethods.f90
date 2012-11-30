  !##############################
  !FEMMethods is the main FEM Module. Here is all subroutine need to do the analysis
  ! To start the FEM call DOFEM subroutine
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################


module FemMethods
  use FEMMath
  use FEMUtility
  implicit none

contains

  subroutine DoFEM(DisplacementVector, Elms,Nodes,Loads,errorFlag)
    integer, intent(inout) :: errorFlag
    type (element), intent(inout) :: Elms(:)
    type (node), intent(in):: Nodes(:)
    type (load), intent(in) :: Loads(:)
    real , intent(out) :: DisplacementVector(:)

    !GTRGConverter = Global To Reduced Global Stiffness Matrix Converter, it converts the indexes
    integer :: status, RGSMLen,i,j, GTRGConverter(DOF*size(Nodes))
    real , ALLOCATABLE :: GStiffnesMatrix(:,:), LoadVector(:)

    if (errorFlag < 0) return

    call SetElementProperties(Elms, Nodes)  
    CALL GToReducedGStiffnessMatrixConverter(GTRGConverter, Nodes)
    RGSMLen = TotalDegreesOfFreedom(Nodes)
    print *, 'DOF         = ', RGSMLen
    allocate (GStiffnesMatrix(RGSMLen,RGSMLen) , LoadVector(RGSMLen), stat=errorFlag)
    if (errorFlag < 0)then
       print *, "***Not Enough Memory*** when allocating in CalcDisplacement "
       return
    end if

    call NullifyRealMatrix(GStiffnesMatrix)
    call NullifyRealVector(DisplacementVector)
    call NullifyRealVector(LoadVector)

    call GlobalStiffness(GStiffnesMatrix,Elms,Nodes,GTRGConverter,errorFlag)

    call PopulateLoads(LoadVector,Loads,GTRGConverter, errorflag)

    if (errorFlag < 0) return

    if (RGSMLen > 1000)then 
        Print *, 'Using GaussSeidel...'
        call GaussSeidel(GStiffnesMatrix,LoadVector,DisplacementVector,RGSMLen,Errorflag)
    else
      Print *, 'Using Gauss Elimination...'
      call GaussSolver(GStiffnesMatrix,LoadVector,DisplacementVector,RGSMLen,Errorflag)
    end if

    if (errorFlag < 0) return
    
    if (pr_switch>3)then
       print * ,''
       print *, '##### GlobalStivhetsmatrise: '
       call PrintMatrix(GStiffnesMatrix)
       print *, '##### Forskyvninger: '
       print *,  DisplacementVector

    end if
    if (allocated(GStiffnesMatrix)) deallocate(GStiffnesMatrix, stat=errorflag)
    if (allocated(LoadVector)) deallocate(LoadVector, stat=errorflag)
    if (errorflag /= 0) print *, "array: Deallocation request denied"
    call CalculateElementForces(Elms, DisplacementVector, GTRGConverter,errorflag)
  end subroutine DoFEM



  subroutine GlobalStiffness(GStiffnesMatrix, Elms,Nodes,GTRGConverter,errorFlag)
    integer ,intent(in)::GTRGConverter(:)
    type (element), intent(in):: Elms(:)
    type (node), intent(in):: Nodes(:)
    integer ,intent(inout) :: errorFlag
    real ,intent(out) :: GStiffnesMatrix(:,:)

    integer :: i ,j, k
    real :: ElmStiffnessMatrix(6,6)
    type (element) :: elm
    ! GCM is an array that converts  indexes from the ElmStiffnessMatrix to the global stiffness matrix
    integer :: GMC(6)  



    if (Errorflag .LT. 0)then
       print *, 'ERRORFLAG AT BEGINING OF GLOBALSTIFFNESS'
       return
    end if

    do i = 1, ubound(Elms,1)
       elm = Elms(i)
       call GlobalElementStiffness(ElmStiffnessMatrix,elm)

       do j=1,DOF
          GMC(j)=GTRGConverter(((elm%node1-1)*DOF) +j)* Nodes(elm%node1)%GDOF(j)
          GMC(j+3)=GTRGConverter(((elm%node2-1)*DOF) +j) *  Nodes(elm%node2)%GDOF(j)
       end do

       if (pr_switch > 6)then
          print * ,''
          print *, '##### GlobalStiffness: '
          print *, 'Jobber på element ...: ', elm
          print *, 'GlobalMatrixConverter... : ' , GMC
       end if

       do j=1,6
          do k =1,6
             if ((GMC(k) == 0) .OR. (GMC(j)==0))  cycle
             GStiffnesMatrix(GMC(k),GMC(j))=GStiffnesMatrix(GMC(k),GMC(j))+ElmStiffnessMatrix(k,j)
          end do
       end do
    end do
    if ((size(Nodes)*3) - size(GStiffnesMatrix,1) < 3)  then
      errorflag = -3
      print * , 'The system has to many degrees of freedom, it us unsolvable'
    end if
  end subroutine GlobalStiffness


  integer Function totalDegreesOfFreedom(Nodes)
    type (node), intent(in):: Nodes(:)

    integer ::i,j

    totalDegreesOfFreedom=0
    do i=1,ubound(Nodes,1)
       do j=1,3
          totalDegreesOfFreedom = totalDegreesOfFreedom+Nodes(i)%GDOF(j)
       end do
    end do
  end Function totalDegreesOfFreedom



  Subroutine CalculateLoadsOnElement(elm)
    type (element), intent(inout) :: elm

    real :: ElmStiffnessMatrix(DOF*2,DOF*2), ElmsRotationMatrix(6,6)

    ElmsRotationMatrix =Transpose(RotationMatrix(elm%cosT,elm%sinT))
    call LocalElementStiffness(ElmStiffnessMatrix, elm)
    elm%ForceVector = matmul(ElmsRotationMatrix, elm%Displacement)
    elm%ForceVector = matmul(ElmStiffnessMatrix, elm%ForceVector)
  end Subroutine CalculateLoadsOnElement


  Subroutine CalculateElementForces(Elms, DisplacementVector, GTRGConverter,errorflag)
    type (element), intent(inout) :: Elms(:)
    real, intent(in) ::  DisplacementVector(:)
    integer , intent(in) :: GTRGConverter(:)
        integer, intent(inout)  :: errorflag

    integer :: i,j

    do i =1,size(Elms)
       do j=1,DOF
          Elms(i)%Displacement(j)=DisplacementVector(GTRGConverter(((Elms(i)%node1-1)*DOF) +j))
          Elms(i)%Displacement(j+3)=DisplacementVector(GTRGConverter(((Elms(i)%node2-1)*DOF) +j) )
       end do
       call CalculateLoadsOnElement(Elms(i))
       call TestInfAndNaN(Elms(i)%ForceVector, errorflag)
        if (errorFlag < 0) then
          print *, 'Got NaN in the ForceVector in element ', i
          return
        end if
    end do

  end Subroutine CalculateElementForces


  subroutine SetElementProperties(Elms, Nodes)
    type (element), intent(inout) :: Elms(:)
    type (node) , intent(in) :: Nodes(:)

    integer :: n
    real :: x1,x2,y1,y2

    do n=1,ubound(Elms,1)
       x2 = Nodes(Elms(n)%node2)%x
       x1=Nodes(Elms(n)%node1)%x
       y2 = Nodes(Elms(n)%node2)%y
       y1= Nodes(Elms(n)%node1)%y
       Elms(n)%l=LengthBetweenPoints(x1,y1,x2,y2)
       Elms(n)%cosT=(x2-x1)/Elms(n)%l
       Elms(n)%sinT=(y2-y1)/Elms(n)%l
    end do
  end subroutine SetElementProperties



  function BiggestNodeCoordinate(Nodes)
    type (node) , intent(in) :: Nodes(:)
    real :: BiggestNodeCoordinate

    integer :: i,temp
  
    BiggestNodeCoordinate=0
    do i=0,ubound(Nodes,1)
      if (abs(Nodes(i)%x) > BiggestNodeCoordinate )then
        BiggestNodeCoordinate = abs(Nodes(i)%x)
      end if
      if (abs(Nodes(i)%y) > BiggestNodeCoordinate ) then
        BiggestNodeCoordinate = abs(Nodes(i)%y)
       end if 
    end do

end function BiggestNodeCoordinate


end module FemMethods
