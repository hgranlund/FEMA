
module FemMethods
  use FEMTypes
  use Math
  implicit none

contains
  !###############################
  !Methoden kalkulerer forsyvnigene til systemet
  !
  !###############################
  subroutine CalcDisplacement(DisplacementVector, Elms,Nodes,Loads,numberOfElm,numberOfNodes,numberOfLoads,errorFlag)

    integer, intent(in) ::  numberOfNodes,numberOfElm,numberOfLoads
    integer, intent(inout) :: errorFlag
    type (element), intent(in) :: Elms(:)
    type (node), intent(in):: Nodes(:)
    type (load), intent(in) :: Loads(:)
    real , intent(out) :: DisplacementVector(:)

    integer :: status, GSMLen,i,j
    real , ALLOCATABLE :: GlobalStiffnessMatrix(:,:), LoadVector(:)

    if (errorFlag .NE. 0)then
       print *, 'Errorflag at begining of CalcDisplacement'
       return
    end if

    GSMLen = totalDegrees(Nodes,numberOfNodes)
    allocate (GlobalStiffnessMatrix(GSMLen,GSMLen) , LoadVector(GSMLen), stat=status)
    if (status .NE. 0)then
       Errorflag =  status
       print *, "***Not Enough Memory*** when allocating in CalcDisplacement "
       return
    end if
    do i=1,GSMLen
       do j=1,GSMLen
          GlobalStiffnessMatrix(i,j)=0
       end do
       DisplacementVector(i)=0
       LoadVector(i)=1
    end do
    call PrintMatrix(GlobalStiffnessMatrix, GSMLen, GSMLen)
    call GlobalStiffness(GlobalStiffnessMatrix,Elms,numberOfElm,Nodes,numberOfNodes,errorFlag)
    print *, 'global etter genereinge'
    call PrintMatrix(GlobalStiffnessMatrix, GSMLen, GSMLen)
    call GaussSolver(GlobalStiffnessMatrix,DisplacementVector,LoadVector,GSMLen,Errorflag)

  end subroutine CalcDisplacement

  !###############################
  !LS LocalStiffnesMatrix er den genererte lokale stivhetsmatrisen
  !Elm er elementet i fokus
  !###############################
  subroutine LocalStiffness(LS, Elm)
    real , intent(out) :: LS(:,:)
    type (element), intent(in):: Elm

    integer::i,j
    real :: EI,EA,L,t1,t2,t3,t4
    character (len=21) :: Tag = '##### LocalStiffness:'

    do i=1,6
       do j=1,6
          LS(i,j)=0
       end do
    end do

    L=Elm%L
    t1 = Elm%EA /L
    t2 = (12*Elm%EI)/L**3
    t3 = (6*Elm%EI)/L**2
    t4= (2*Elm%EI)/L
    if (pr_switch > 7)then
       print *,''
       print * , Tag
       print *, "t1:", t1, "t2:", t2, "t3:", t3, "t4:", t4
    end if
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
    LS(6,3)=2*t1
    LS(3,6)=2*t1
    LS(2,6)=-t3 
    LS(6,2)=-t3
    LS(5,6)=t3
    LS(6,5)=t3
  end subroutine LocalStiffness


  !###############################
  !En prosedyre som genererer  den globale stivhetsmatrisen
  !###############################
  subroutine GlobalStiffness(GlobalStiffnessMatrix, Elms,numberOfElm,Nodes,numberOfNodes,errorFlag)
    integer ,intent(in)::numberOfElm,numberOfNodes
    integer ,intent(inout) :: errorFlag
    real , intent(out) :: GlobalStiffnessMatrix(:,:)
    type (element), intent(in):: Elms(:)
    type (node), intent(in):: Nodes(:)


    integer :: i ,j, k
    real :: LocalStiffnessMatrix(6,6)
    type (element) :: elm
    integer :: GMC(6) , GTRGConverter(DOF*numberOfNodes)
    character (Len=23) :: Tag = '##### GlobalStiffness: '



    if (errorFlag .NE. 0)then
       print *, 'Errorflag at begining of GlobalStiffness'
       return
    end if
    
    call GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter, Nodes, numberOfNodes)

    do i = 1, numberOfElm, 1
       elm = Elms(i)

       call LocalStiffness(LocalStiffnessMatrix,elm)

       ! Hvis GobalMartixConverter (GCM) er null Betyr det at...
       ! verdien ikke skal være med videre pga. grensebetingerlser
       do j=1,DOF,1
          GMC(j)=GTRGConverter(((elm%node1-1)*DOF) +j)* Nodes(elm%node1)%GDOF(j)
          GMC(j+3)=GTRGConverter(((elm%node2-1)*DOF) +j) *  Nodes(elm%node2)%GDOF(j)
       end do
       if (pr_switch > 6)then
          print * ,''
          print *, Tag
          print *, 'Jobber på element ...: ', elm
          print *, 'GlobalMatrixConverter... : ' , GMC
       end if

       do j=1,6,1
          do k =1,6,1
             if (GMC(k) == 0) cycle
             GlobalStiffnessMatrix(GMC(k),GMC(j))=GlobalStiffnessMatrix(GMC(k),GMC(j))+LocalStiffnessMatrix(k,j)
          end do
       end do
       if(pr_switch >8)then
          print *, ''
          print *,'LocalStiffnessMatrix at element, ' ,i
          call PrintMatrix(LocalStiffnessMatrix,6,6)
          print *, 'GlobalStiffnessMatrix at element, ', i
          call PrintMatrix(GlobalStiffnessMatrix,9,9)

       end if
    end do
  end subroutine GlobalStiffness

  Subroutine GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter,Nodes, numberOfNodes)
    type (node), intent(in):: Nodes(:)
    integer ,intent(in)::numberOfNodes
    integer ,intent(out) :: GTRGConverter(9)

    integer ::i,j,globalIndex,redusedIndex

    globalIndex=1
    redusedIndex=1
    do i=1,numberOfNodes
       do j=1,3
          if (Nodes(i)%GDOF(j) .NE. 0)then
             GTRGConverter(globalIndex)=redusedIndex
             redusedIndex=redusedIndex+1
          end if
          globalIndex=globalIndex+1
       end do
    end do
  end Subroutine GlobalToRedusedGlobalStiffnessMatrixConverter

  integer Function totalDegrees(Nodes, numberOfNodes)
    type (node), intent(in):: Nodes(:)
    integer ,intent(in)::numberOfNodes

    integer ::i,j

    totalDegrees=0
    do i=1,numberOfNodes
       do j=1,3
          totalDegrees = totalDegrees+Nodes(i)%GDOF(j)
       end do
    end do
  end Function totalDegrees


end module FemMethods
