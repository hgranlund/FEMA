  !##############################
  !
  !
  ! Author: Simen Haugerud Granlund
  ! Date/version: 02-11-12/ 1.0
  !###############################


module FemMethods
  use FEMTypes
  use FEMMath
  use FEMUtility
  implicit none

contains

  subroutine DoFEA(DisplacementVector, Elms,Nodes,Loads,errorFlag)
    integer, intent(inout) :: errorFlag
    type (element), intent(inout) :: Elms(:)
    type (node), intent(in):: Nodes(:)
    type (load), intent(in) :: Loads(:)
    real , intent(out) :: DisplacementVector(:)


    integer :: status, RGSMLen,i,j, GTRGConverter(DOF*size(Nodes))
    real , ALLOCATABLE :: GlobalStiffnessMatrix(:,:), LoadVector(:)

    if (errorFlag .NE. 0) return

    call SetElementProperties(Elms, Nodes)  
    CALL GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter, Nodes)
    RGSMLen = TotalDegreesOfFreedom(Nodes)
    allocate (GlobalStiffnessMatrix(RGSMLen,RGSMLen) , LoadVector(RGSMLen), stat=errorFlag)
    if (errorFlag .NE. 0)then
       print *, "***Not Enough Memory*** when allocating in CalcDisplacement "
       return
    end if

    call NullifyRealMatrix(GlobalStiffnessMatrix)
    call NullifyRealVector(DisplacementVector)
    call NullifyRealVector(LoadVector)

    call GlobalStiffness(GlobalStiffnessMatrix,Elms,Nodes,GTRGConverter,errorFlag)
    call PopulateLoads(LoadVector,Loads,GTRGConverter, errorflag)
    call GaussSolver(GlobalStiffnessMatrix,LoadVector,DisplacementVector,RGSMLen,Errorflag)

    if (pr_switch>2)then
       print * ,''
       print *, '##### GlobalStivhetsmatrise: '
       call PrintMatrix(GlobalStiffnessMatrix)
       print *, 'loadvector: ', LoadVector
       print *, '##### Forskyvninger: '
       print *,  DisplacementVector
    end if
    call CalculateElementForces(Elms, DisplacementVector, GTRGConverter)
  end subroutine DoFEA



  !LS LocalStiffnesMatrix er den genererte lokale stivhetsmatrisen
  !Elm er elementet i fokus

  subroutine LocalStiffness(LS, Elm)
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
    LS(2,2)=t2
    LS(3,3)=2*t4
    LS(4,4)=t1  
    LS(5,5)=t2
    LS(6,6)=2*t4
    LS(4,1)=-t1
    LS(1,4)=-t1
    LS(2,5)=-t2
    LS(5,2)=-t2
    LS(5,3)=t3
    Ls(3,5)=t3
    LS(2,3)=-t3
    LS(3,2)=-t3
    LS(6,3)=t4
    LS(3,6)=t4
    LS(2,6)=-t3 
    LS(6,2)=-t3
    LS(5,6)=t3
    LS(6,5)=t3

    if (pr_switch > 7)then
       print *,''
       print * , '##### LocalStiffness:'
       print *, "t1:", t1, "t2:", t2, "t3:", t3, "t4:", t4
       call PrintMatrix(LS)
    end if
  end subroutine LocalStiffness


  !LocalStiffnessWithRotation genererer den globale stivhetsmatrisen til elmentet (elm) direkte

  subroutine LocalStiffnessWithRotation(LS, elm)
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
       print *,"C: ",c," S: ", s ," I: ",inertia, " L:",l,  &
            &"(12 *I) /L**2:", t1, "(6*I)/L:", t2, "E/L:", t3
       call PrintMatrix(LS)
    end if

  end subroutine LocalStiffnessWithRotation




  !En prosedyre som genererer  den reduserte globale stivhetsmatrisen direkte


  subroutine GlobalStiffness(GlobalStiffnessMatrix, Elms,Nodes,GTRGConverter,errorFlag)
    integer ,intent(in)::GTRGConverter(:)
    type (element), intent(in):: Elms(:)
    type (node), intent(in):: Nodes(:)
    integer ,intent(inout) :: errorFlag
    real ,intent(out) :: GlobalStiffnessMatrix(:,:)

    integer :: i ,j, k
    real :: LocalStiffnessMatrix(6,6)
    type (element) :: elm
    integer :: GMC(6)   ! GCM er en konverteringsmatrise som konverterer
    ! den locale stivhetsmatrisen til den globale stivhetsmatrisen

    if (Errorflag .LT. 0)then
       print *, 'ERRORFLAG AT BEGINING OF GLOBALSTIFFNESS'
       return
    end if

    do i = 1, ubound(Elms,1)
       elm = Elms(i)
       call LocalStiffnessWithRotation(LocalStiffnessMatrix,elm)

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
             GlobalStiffnessMatrix(GMC(k),GMC(j))=GlobalStiffnessMatrix(GMC(k),GMC(j))+LocalStiffnessMatrix(k,j)
          end do
       end do
    end do
  end subroutine GlobalStiffness


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




  ! Genererer en konverteringsmatrise som konverterer fra den globalestivhetsmatrisen til den reduserte globalestivhetsmatrisen.
  ! Den reduserte globlaestivhetsmatrisen er alle grensebetingelser tatt i be


  Subroutine GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter,Nodes)
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
  end Subroutine GlobalToRedusedGlobalStiffnessMatrixConverter


  ! Kalkulerer hvor mange grader av frihet alle nodene har tilsammen

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


  ! Prosedyren kalkulerer kreftene til elementet ut i fra forskyvningene 

  Subroutine CalculateLoadsOnElement(elm)
    type (element), intent(inout) :: elm

    real :: LocalStiffnessMatrix(DOF*2,DOF*2), ElmsRotationMatrix(6,6)

    ElmsRotationMatrix =Transpose(RotationMatrix(elm%cosT,elm%sinT))
    call LocalStiffness(LocalStiffnessMatrix, elm)
    elm%ForceVector = matmul(ElmsRotationMatrix, elm%Displacement)
    elm%ForceVector = matmul(LocalStiffnessMatrix, elm%ForceVector)
  end Subroutine CalculateLoadsOnElement




  ! Prosedyren kalkulerer kreftene på alle elementet ut i fra forskyvningene og setter de locale forskyvningene til hvert element

  Subroutine CalculateElementForces(Elms, DisplacementVector, GTRGConverter)
    type (element), intent(inout) :: Elms(:)
    real, intent(in) ::  DisplacementVector(:)
    integer , intent(in) :: GTRGConverter(:)

    integer :: i,j


    do i =1,size(Elms)
       do j=1,DOF
          Elms(i)%Displacement(j)=DisplacementVector(GTRGConverter(((Elms(i)%node1-1)*DOF) +j))
          Elms(i)%Displacement(j+3)=DisplacementVector(GTRGConverter(((Elms(i)%node2-1)*DOF) +j) )
       end do
       call CalculateLoadsOnElement(Elms(i))
       if(pr_switch>5)then
          print * ,''
          print *,'Krefter på element nr : ', i
          print *, Elms(i)%ForceVector
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





  ! BiggestNodeCoordinate retunerer den største node kordinaten 

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
