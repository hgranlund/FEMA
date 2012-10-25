module FemMethods
  use FEMTypes
  use Math
  implicit none

  ! interface 
  !      subroutine GaussSolver(A,B,X,len, errorFlag) 
  !        integer, intent(in) :: len
  !        integer, intent(inout) :: errorFlag
  !        real, intent(inout)  :: A(len,len), B(len)
  !        REAL, intent(out) :: X(len)
  !      end Subroutine GaussSolver

  !      Subroutine PrintMatrix(A,l,b)
  !        real, intent(inout) :: A(:,:)
  !        integer , intent(in)::l,b
  !      end Subroutine PrintMatrix
  !   end interface

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


    integer :: status, GSMLen,i,j, GTRGConverter(DOF*numberOfNodes)
    real , ALLOCATABLE :: GlobalStiffnessMatrix(:,:), LoadVector(:)

    if (errorFlag .NE. 0)then
       print *, 'Errorflag at begining of CalcDisplacement'
       return
    end if
    CALL GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter, Nodes, numberOfNodes)
    GSMLen = totalDegrees(Nodes,numberOfNodes)
    allocate (GlobalStiffnessMatrix(GSMLen,GSMLen) , LoadVector(GSMLen), stat=status)
    if (status .NE. 0)then
       Errorflag =  status
       print *, "***Not Enough Memory*** when allocating in CalcDisplacement "
       return
    end if

!initilise
    do i=1,GSMLen
       do j=1,GSMLen
          GlobalStiffnessMatrix(i,j)=0
       end do
       DisplacementVector(i)=0
       LoadVector(i)=0
    end do

    call GlobalStiffness(GlobalStiffnessMatrix,Elms,numberOfElm,Nodes,GTRGConverter,errorFlag)
    call PopulateLoads(LoadVector,Loads,GTRGConverter, numberOfLoads,errorflag)
    if (pr_switch>5)then
       print * ,''
       print *, '##### GlobalStivhetsmatrise: '
       call PrintMatrix(GlobalStiffnessMatrix, GSMLen, GSMLen)
    end if

    call GaussSolver(GlobalStiffnessMatrix,LoadVector,DisplacementVector,GSMLen,Errorflag)
    if (pr_switch>5)then 
       print *,''
       print *,'Displacemant:'
       print *,DisplacementVector
    end if
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
  subroutine GlobalStiffness(GlobalStiffnessMatrix, Elms,numberOfElm,Nodes,GTRGConverter,errorFlag)
    integer ,intent(in)::numberOfElm, GTRGConverter(:)
    type (element), intent(in):: Elms(:)
    type (node), intent(in):: Nodes(:)
    integer ,intent(inout) :: errorFlag
    real ,intent(out) :: GlobalStiffnessMatrix(:,:)



    integer :: i ,j, k
    real :: LocalStiffnessMatrix(6,6)
    type (element) :: elm
    integer :: GMC(6)  



    IF (Errorflag .LT. 0)THEN
       PRINT *, 'ERRORFLAG AT BEGINING OF GLOBALSTIFFNESS'
       RETURN
    END IF




    do i = 1, numberOfElm, 1
       elm = Elms(i)

       call LocalStiffness(LocalStiffnessMatrix,elm)

       ! Hvis GobalMartixConverter (GCM) er null Betyr det at...
       ! verdien ikke skal være med videre pga. grensebetingerlser
       ! TODO: her kan vi spare tid ved å lage GMc av mindre rank, slik at vi bare tar med de vardiene vi trenger. Da kan vi fjerne if checken i loop
       do j=1,DOF,1
          GMC(j)=GTRGConverter(((elm%node1-1)*DOF) +j)* Nodes(elm%node1)%GDOF(j)
          GMC(j+3)=GTRGConverter(((elm%node2-1)*DOF) +j) *  Nodes(elm%node2)%GDOF(j)
       end do
       if (pr_switch > 6)then
          print * ,''
          print *, '##### GlobalStiffness: '
          print *, 'Jobber på element ...: ', elm
          print *, 'GlobalMatrixConverter... : ' , GMC
       end if
       !call PrintMatrix(LocalStiffnessMatrix,6,6)
       do j=1,6
          do k =1,6
             if ((GMC(k) == 0) .OR. (GMC(j)==0))  cycle
             GlobalStiffnessMatrix(GMC(k),GMC(j))=GlobalStiffnessMatrix(GMC(k),GMC(j))+LocalStiffnessMatrix(k,j)
          end do
       end do
    end do
  end subroutine GlobalStiffness


  !###############################
  ! Prosedyren populerer Kraftvektoren (LoadVectors)
  !###############################
  subroutine PopulateLoads(LoadVector,Loads,GTRGConverter, numberOfLoads,errorflag)
    integer, intent(in) :: GTRGConverter(:),numberOfLoads,Errorflag
    type (load), intent(in) :: Loads(:)
    real , intent(out):: LoadVector(:)

    integer :: i, globalIndex
    type (load):: tempLoad

    if (Errorflag .LT. 0) return

    do i=1,numberOfLoads
       tempLoad= Loads(i)
       globalIndex=(tempLoad%nodeNr-1)*3+tempLoad%DOF
       LoadVector(GTRGConverter(globalIndex))=tempLoad%value
      
    end do

    if (pr_switch>5)then
       print *, '##### PopulateLoads:'
       print *, 'LoadVector', LoadVector
    end if
       
  end subroutine PopulateLoads





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
