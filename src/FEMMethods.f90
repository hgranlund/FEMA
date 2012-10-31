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
  !###############################

  subroutine CalcDisplacement(DisplacementVector, Elms,Nodes,Loads,numberOfElm,numberOfNodes,numberOfLoads,errorFlag)

    integer, intent(in) ::  numberOfNodes,numberOfElm,numberOfLoads
    integer, intent(inout) :: errorFlag
    type (element), intent(inout) :: Elms(:)
    type (node), intent(in):: Nodes(:)
    type (load), intent(in) :: Loads(:)
    real , intent(out) :: DisplacementVector(:)


    integer :: status, RGSMLen,i,j, GTRGConverter(DOF*numberOfNodes)
    real , ALLOCATABLE :: GlobalStiffnessMatrix(:,:), LoadVector(:)

    if (errorFlag .NE. 0)then
       print *, 'Errorflag at begining of CalcDisplacement'
       return
    end if

    CALL GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter, Nodes, numberOfNodes)
    RGSMLen = totalDegrees(Nodes,numberOfNodes)
    allocate (GlobalStiffnessMatrix(RGSMLen,RGSMLen) , LoadVector(RGSMLen), stat=status)
    if (status .NE. 0)then
       Errorflag =  status
       print *, "***Not Enough Memory*** when allocating in CalcDisplacement "
       return
    end if

    do i=1,RGSMLEN
       do j=1,RGSMLEN
          GlobalStiffnessMatrix(i,j)=0
       end do
       DisplacementVector(i)=0
       LoadVector(i)=0
    end do

    call GlobalStiffness(GlobalStiffnessMatrix,Elms,numberOfElm,Nodes,GTRGConverter,errorFlag)
    call PopulateLoads(LoadVector,Loads,GTRGConverter, numberOfLoads,errorflag)
    if (pr_switch>4)then
       print * ,''
       print *, '##### GlobalStivhetsmatrise: '
       call PrintMatrix(GlobalStiffnessMatrix, RGSMLen, RGSMLen)
       print *, 'loadvector: ', LoadVector
    end if

    call GaussSolver(GlobalStiffnessMatrix,LoadVector,DisplacementVector,RGSMLen,Errorflag)
    call SetElementForces(Elms, DisplacementVector, GTRGConverter,numberOfElm)
  end subroutine CalcDisplacement


 !###############################
  !LS LocalStiffnesMatrix er den genererte lokale stivhetsmatrisen
  !Elm er elementet i fokus
  !###############################

  subroutine LocalStiffness(LS, Elm)
    real , intent(out) :: LS(:,:)
    type (element), intent(in):: Elm

    integer::i,j
    real :: ei,ea,l,t1,t2,t3,t4

    forall (i=1:6, j=1:6) LS(i,j)=0

    ea=elm%e*Elm%a
    ei=elm%e*elm%i
    l=elm%l
    t1 = ea /l
    t2 = (12*ei)/l**3
    t3 = (6*ei)/l**2
    t4= (2*ei)/l
    if (pr_switch > 7)then
       print *,''
       print * , '##### LocalStiffness:'
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



  !##############################
  !LS LocalStiffnesMatrix er den genererte lokale stivhetsmatrisen
  !,til element elm, multiplisert med Elementets rotasjonsmatrise
  !###############################

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

!     !Fix av avrundinertiagsfeil i real verdier
!     if ( abs(c) .LE. epsilon(c) ) c=0 
!     if ( abs(s) .LE. epsilon(s) ) s=0

    if (pr_switch > 7)then
       print *,''
       print * , '##### LocalStiffness:'
       print *,"C: ",c," S: ", s ," I: ",inertia, " L:",l,  &
       &"(12 *I) /L**2:", t1, "(6*I)/L:", t2, "E/L:", t3
    end if

    LS(1,1)=t3*((a*c**2)+(t1*s**2))
    LS(2,1)=t3*(a-t1)*c*s
    LS(3,1)=t3*(-t2*s)
    LS(4,1)=-LS(1,1)
    LS(5,1)=t3*((12/l**2-a)*c*s)
    LS(6,1)=LS(3,1)

    LS(1,2)=LS(2,1)
    LS(2,2)=t3*((a*s**2)+(t1*c**2))
    LS(3,2)=t3* (6*inertia*c/l)
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
    LS(6,4)=LS(6,1)

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

    if ( pr_switch >9 ) then
      print *, '###### LocalStiffnessMatrix'
      call PrintMatrix(LS,6,6)
    end if

  end subroutine LocalStiffnessWithRotation


  !###############################
  !En prosedyre som genererer  den reduserte globale stivhetsmatrisen direkte
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
    integer :: GMC(6)   ! GCM er en konverteringsmatrise som konverterer
                        ! den locale stivhetsmatrisen til den globale stivhetsmatrisen

    if (Errorflag .LT. 0)then
       print *, 'ERRORFLAG AT BEGINING OF GLOBALSTIFFNESS'
       return
    end if

    do i = 1, numberOfElm
       elm = Elms(i)
       call LocalStiffnessWithRotation(LocalStiffnessMatrix,elm)
       ! Hvis GobalMartixConverter (GCM) er null Betyr det at
       ! verdien ikke skal være med videre pga. grensebetingerlser
       ! TODO: her kan vi spare tid ved å lage GMc av mindre rank, slik at vi bare tar med de vardiene vi trenger. Da kan vi fjerne if checken i loop
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
        print *,tempLoad
       globalIndex=(tempLoad%nodeNr-1)*3+tempLoad%DOF
       LoadVector(GTRGConverter(globalIndex))=tempLoad%value
    end do

    if (pr_switch>5)then
       print *, '##### PopulateLoads:'
       print *, 'LoadVector', LoadVector
    end if

  end subroutine PopulateLoads



  !###############################
  ! Genererer en konverteringsmatrise som konverterer fra den globalestivhetsmatrisen til den reduserte globalestivhetsmatrisen.
  ! Den reduserte globlaestivhetsmatrisen er alle grensebetingelser tatt i be
  !###############################

  Subroutine GlobalToRedusedGlobalStiffnessMatrixConverter(GTRGConverter,Nodes, numberOfNodes)
    type (node), intent(in):: Nodes(:)
    integer ,intent(in)::numberOfNodes
    integer ,intent(out) :: GTRGConverter(numberOfNodes*3)

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



    !###############################
    ! Kalkulerer hvor mange grader av frihet alle nodene har tilsammen
    !###############################

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


  !###############################
  ! Prosedyren kalkulerer kreftene til elementet ut i fra forskyvningene
  !###############################

  Subroutine LoadsOnElement(ElementLoadVector,elm, ElementDisplacementVector)
    real, intent(in) :: ElementDisplacementVector(:) 
    type (element), intent(in) :: elm
    real, intent(out):: ElementLoadVector(:) 
    
    real :: LocalStiffness (DOF*2,DOF*2)

    call LocalStiffnessWithRotation(LocalStiffness, elm)
    ElementLoadVector = matmul(LocalStiffness, ElementDisplacementVector)
  end Subroutine LoadsOnElement


  !###############################
  ! Prosedyren kalkulerer kreftene til alle elementet ut i fra forskyvningene
  !###############################

  Subroutine SetElementForces(Elms, DisplacementVector, GTRGConverter,numberOfElm)
    type (element), intent(inout) :: Elms(:)
    real, intent(in) ::  DisplacementVector(:)
    integer , intent(in) :: GTRGConverter(:), numberOfElm

    integer :: i,j
    real ::  ElementDisplacementVector(DOF*2)

    type (element) :: elm

    do i =1,numberOfElm
      elm = Elms(i)
       do j=1,DOF
          ElementDisplacementVector(j)=DisplacementVector(GTRGConverter(((elm%node1-1)*DOF) +j))
          ElementDisplacementVector(j+3)=DisplacementVector(GTRGConverter(((elm%node2-1)*DOF) +j) )
       end do
      call LoadsOnElement(elm%ForceVector, elm, ElementDisplacementVector )
      print *, (elm%ForceVector(j), j=1,6)
    end do
    
  end Subroutine SetElementForces




  !###############################
  ! Prosedyren populerer verdiene til elementene
  !###############################

  subroutine SetElementProperties(Elms, Nodes, numberOfElm)
  
    type (element), intent(inout) :: Elms(:)
    type (node) , intent(in) :: Nodes(:)
    integer, intent(in)  :: numberOfElm

    integer :: n
    real :: length, dx,dy

    do n=1,numberOfElm
      dx = Nodes(Elms(n)%node2)%x-Nodes(Elms(n)%node1)%x
      dy = Nodes(Elms(n)%node2)%y-Nodes(Elms(n)%node1)%y
      length=sqrt(dx**2+dy**2)
      Elms(n)%cosT=(dx)/length
      Elms(n)%sinT=(dy)/length
      print *, Elms(n)%cosT,  Elms(n)%sinT
    end do
  end subroutine SetElementProperties

end module FemMethods
