MODULE LinkedListMod
USE ListMod,  ONLY : Allocation_List, Pointer_List
IMPLICIT NONE


TYPE Allocation_LinkedList

  PRIVATE  
  CLASS(Allocation_List), ALLOCATABLE :: HeadList
  CLASS(Allocation_List), ALLOCATABLE :: CurrList
  CLASS(Allocation_List), POINTER :: TailList  
  
  CONTAINS
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsSet     => IsSet_AllocationLinkedList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Initiate  => Initiate_AllocationLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Connect   => Connect_AllocationLinkedList

    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: GetCurr   => GetCurrent_AllocationLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: ForCurr   => ForwardCurrent_AllocationLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: DispCurr  => DisplayCurrent_AllocationLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: ResetCurr => ResetCurrent_AllocationLinkedList
        
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Reset     => Reset_AllocationLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Display   => Display_AllocationLinkedList


END TYPE Allocation_LinkedList

TYPE Pointer_LinkedList

  PRIVATE  
  CLASS(Pointer_List), POINTER :: HeadList => NULL()
  CLASS(Pointer_List), POINTER :: CurrList => NULL()
  CLASS(Pointer_List), POINTER :: TailList => NULL()  
  
  CONTAINS
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsSet     => IsSet_PointerLinkedList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Initiate  => Initiate_PointerLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Connect   => Connect_PointerLinkedList

    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: GetCurr   => GetCurrent_PointerLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: ForCurr   => ForwardCurrent_PointerLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: DispCurr  => DisplayCurrent_PointerLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: ResetCurr => ResetCurrent_PointerLinkedList
        
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Reset     => Reset_PointerLinkedList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Display   => Display_PointerLinkedList


END TYPE Pointer_LinkedList


PRIVATE :: IsSet_AllocationLinkedList, Initiate_AllocationLinkedList, Connect_AllocationLinkedList,&
            GetCurrent_AllocationLinkedList, ForwardCurrent_AllocationLinkedList,&
            DisplayCurrent_AllocationLinkedList, ResetCurrent_AllocationLinkedList,&
              Reset_AllocationLinkedList, Display_AllocationLinkedList, &
           IsSet_PointerLinkedList, Initiate_PointerLinkedList, Connect_PointerLinkedList,&
            GetCurrent_PointerLinkedList, ForwardCurrent_PointerLinkedList,&
            DisplayCurrent_PointerLinkedList, ResetCurrent_PointerLinkedList,&
             Reset_PointerLinkedList, Display_PointerLinkedList

PUBLIC  :: Allocation_LinkedList, Pointer_LinkedList


CONTAINS


LOGICAL FUNCTION IsSet_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(IN) :: self


IsSet_AllocationLinkedList = ALLOCATED(self%HeadList) .AND. ALLOCATED(self%CurrList) .AND. ASSOCIATED(self%TailList)

END FUNCTION IsSet_AllocationLinkedList

LOGICAL FUNCTION IsSet_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(IN) :: self


IsSet_PointerLinkedList = ASSOCIATED(self%HeadList) .AND. ASSOCIATED(self%CurrList) .AND. ASSOCIATED(self%TailList)

END FUNCTION IsSet_PointerLinkedList

SUBROUTINE Initiate_AllocationLinkedList(self, AllocationListValue)

CLASS(Allocation_LinkedList), TARGET, INTENT(INOUT) :: self
CLASS(Allocation_List),               INTENT(IN)    :: AllocationListValue


IF(.NOT. self%IsSet()) THEN 
  ALLOCATE(self%HeadList, SOURCE=AllocationListValue)
  ALLOCATE(self%CurrList, SOURCE=self%HeadList)
  self%TailList => self%HeadList
ELSE
  print*,  "Linked List Already Set"
END IF

END SUBROUTINE Initiate_AllocationLinkedList

SUBROUTINE Initiate_PointerLinkedList(self, PointerListValue)

CLASS(Pointer_LinkedList),          INTENT(INOUT) :: self
TYPE(Pointer_List),        POINTER, INTENT(IN)    :: PointerListValue


IF(.NOT. self%IsSet()) THEN 
  self%HeadList => PointerListValue
  self%CurrList => PointerListValue
  self%TailList => PointerListValue
ELSE
  print*,  "Linked List Already Set"
END IF

END SUBROUTINE Initiate_PointerLinkedList

SUBROUTINE Connect_AllocationLinkedList(self, AllocationListValue)

CLASS(Allocation_LinkedList),         INTENT(INOUT) :: self
CLASS(Allocation_List),       TARGET, INTENT(IN)    :: AllocationListValue


IF(self%IsSet()) THEN
  CALL self%TailList%SetNext(AllocationListValue)
  self%TailList => self%TailList%GetNext()
  ALLOCATE(self%CurrList, SOURCE=self%HeadList)
ELSE
  print*,  "Linked List Not Set"
END IF

END SUBROUTINE Connect_AllocationLinkedList

SUBROUTINE Connect_PointerLinkedList(self, PointerListValue)

CLASS(Pointer_LinkedList),         INTENT(INOUT) :: self
CLASS(Pointer_List),       TARGET, INTENT(IN)    :: PointerListValue


IF(self%IsSet()) THEN
  CALL self%TailList%SetNext(PointerListValue)
  self%TailList => self%TailList%GetNext()
ELSE
  print*,  "Linked List Not Set"
END IF

END SUBROUTINE Connect_PointerLinkedList

FUNCTION GetCurrent_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(IN)  :: self
CLASS(Allocation_List),       ALLOCATABLE :: GetCurrent_AllocationLinkedList


IF(self%IsSet()) THEN
  ALLOCATE(GetCurrent_AllocationLinkedList, SOURCE=self%CurrList)
ELSE
  print*,  "Linked List Not Set"
END IF

END FUNCTION GetCurrent_AllocationLinkedList

FUNCTION GetCurrent_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(IN) :: self
CLASS(Pointer_List),       POINTER    :: GetCurrent_PointerLinkedList


IF(self%IsSet()) THEN
  GetCurrent_PointerLinkedList => self%CurrList
ELSE
  print*,  "Linked List Not Set"
END IF

END FUNCTION GetCurrent_PointerLinkedList

SUBROUTINE ForwardCurrent_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(INOUT) :: self

CLASS(Allocation_List),       ALLOCATABLE   :: TempAllocationList


IF(self%IsSet() .AND. self%CurrList%IsNext()) THEN
  ALLOCATE(TempAllocationList, SOURCE=self%CurrList%GetNext())
  DEALLOCATE(self%CurrList)
  ALLOCATE(self%CurrList, SOURCE=TempAllocationList) 
ELSE
  print*,  "Next List not Set"
END IF

END SUBROUTINE ForwardCurrent_AllocationLinkedList

SUBROUTINE ForwardCurrent_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(INOUT) :: self


IF(self%IsSet() .AND. self%CurrList%IsNext()) THEN
  self%CurrList => self%CurrList%GetNext()
ELSE
  print*,  "Next List not Set"
END IF

END SUBROUTINE ForwardCurrent_PointerLinkedList

SUBROUTINE DisplayCurrent_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(IN) :: self


CALL self%CurrList%Display()

END SUBROUTINE DisplayCurrent_AllocationLinkedList

SUBROUTINE DisplayCurrent_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(IN) :: self


CALL self%CurrList%Display()

END SUBROUTINE DisplayCurrent_PointerLinkedList

SUBROUTINE ResetCurrent_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(INOUT) :: self


IF(self%IsSet()) THEN
  DEALLOCATE(self%CurrList)
  ALLOCATE(self%CurrList, SOURCE=self%HeadList)
ELSE
  print*,  "Linked List Not Set"
END IF

END SUBROUTINE ResetCurrent_AllocationLinkedList

SUBROUTINE ResetCurrent_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(INOUT) :: self


IF(self%IsSet()) THEN
  self%CurrList => self%HeadList
ELSE
  print*,  "Linked List Not Set"
END IF

END SUBROUTINE ResetCurrent_PointerLinkedList

SUBROUTINE Reset_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), INTENT(INOUT) :: self


DEALLOCATE(self%HeadList)
DEALLOCATE(self%CurrList)
NULLIFY(self%TailList)

END SUBROUTINE Reset_AllocationLinkedList

SUBROUTINE Reset_PointerLinkedList(self)

CLASS(Pointer_LinkedList), INTENT(INOUT) :: self


NULLIFY(self%HeadList)
NULLIFY(self%CurrList)
NULLIFY(self%TailList)

END SUBROUTINE Reset_PointerLinkedList

SUBROUTINE Display_AllocationLinkedList(self)

CLASS(Allocation_LinkedList), TARGET, INTENT(IN) :: self

CLASS(Allocation_List),       POINTER            :: Iter


Iter => self%HeadList
DO
  CALL Iter%Display()
  IF(Iter%IsNext()) THEN
    Iter => Iter%GetNext()
  ELSE
    EXIT
  END IF
END DO

END SUBROUTINE Display_AllocationLinkedList

SUBROUTINE Display_PointerLinkedList(self)

CLASS(Pointer_LinkedList), TARGET, INTENT(IN) :: self

CLASS(Pointer_List),       POINTER            :: Iter


Iter => self%HeadList
DO
  CALL Iter%Display()
  IF(Iter%IsNext()) THEN
    Iter => Iter%GetNext()
  ELSE
    EXIT
  END IF
END DO

END SUBROUTINE Display_PointerLinkedList

END MODULE LinkedListMod
