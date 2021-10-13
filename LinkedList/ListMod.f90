MODULE ListMod
IMPLICIT NONE


TYPE Allocation_List   

  PRIVATE  
  CLASS(*), ALLOCATABLE :: Item
  CLASS(Allocation_List), ALLOCATABLE :: Next
  
  CONTAINS
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsItem   => IsItem_AllocationList
    PROCEDURE, PASS(self), PUBLIC                  :: SetItem  => SetItem_AllocationList
    PROCEDURE, PASS(self), PUBLIC                  :: GetItem  => GetItem_AllocationList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: FreeItem => FreeItem_AllocationList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsNext   => IsNext_AllocationList
    PROCEDURE, PASS(self), PUBLIC                  :: SetNext  => SetNext_AllocationList
    PROCEDURE, PASS(self), PUBLIC                  :: GetNext  => GetNext_AllocationList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: FreeNext => FreeNext_AllocationList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Reset    => Reset_AllocationList
    
    PROCEDURE, PASS(self), PUBLIC                  :: Display  => Display_AllocationListItem

END TYPE Allocation_List

TYPE Pointer_List   

  PRIVATE  
  CLASS(*), POINTER :: Item
  CLASS(Pointer_List), POINTER :: Next => NULL()
  
  CONTAINS
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsItem   => IsItem_PointerList  
    PROCEDURE, PASS(self), PUBLIC                  :: SetItem  => SetItem_PointerList
    PROCEDURE, PASS(self), PUBLIC                  :: GetItem  => GetItem_PointerList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: FreeItem => FreeItem_PointerList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: IsNext   => IsNext_PointerList
    PROCEDURE, PASS(self), PUBLIC                  :: SetNext  => SetNext_PointerList
    PROCEDURE, PASS(self), PUBLIC                  :: GetNext  => GetNext_PointerList
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: FreeNext => FreeNext_PointerList
    
    PROCEDURE, PASS(self), PUBLIC, NON_OVERRIDABLE :: Reset    => Reset_PointerList
    
    PROCEDURE, PASS(self), PUBLIC                  :: Display  => Display_PointerListItem

END TYPE Pointer_List


PRIVATE :: IsItem_AllocationList, SetItem_AllocationList, GetItem_AllocationList, FreeItem_AllocationList,&
            IsNext_AllocationList, SetNext_AllocationList, GetNext_AllocationList, FreeNext_AllocationList,&
             Reset_AllocationList, Display_AllocationListItem,&
             
           IsItem_PointerList, SetItem_PointerList, GetItem_PointerList, FreeItem_PointerList,&
            IsNext_PointerList, SetNext_PointerList, GetNext_PointerList, FreeNext_PointerList,&
             Reset_PointerList, Display_PointerListItem

PUBLIC  :: Allocation_List, Pointer_List


CONTAINS


LOGICAL FUNCTION IsItem_AllocationList(self)

CLASS(Allocation_List), INTENT(IN) :: self


IsItem_AllocationList = ALLOCATED(self%Item)

END FUNCTION IsItem_AllocationList

LOGICAL FUNCTION IsItem_PointerList(self)

CLASS(Pointer_List), INTENT(IN) :: self


IsItem_PointerList = ASSOCIATED(self%Item)

END FUNCTION IsItem_PointerList

SUBROUTINE SetItem_AllocationList(self, ItemValue)

CLASS(Allocation_List), INTENT(INOUT) :: self
CLASS(*),               INTENT(IN)    :: ItemValue


IF(.NOT. self%IsItem()) THEN
  ALLOCATE(self%Item, SOURCE=ItemValue)
ELSE
  print*, "List Item Already Set"
END IF

END SUBROUTINE SetItem_AllocationList

SUBROUTINE SetItem_PointerList(self, ItemValue)

CLASS(Pointer_List), INTENT(INOUT) :: self
CLASS(*),    TARGET, INTENT(IN)    :: ItemValue


IF(.NOT. self%IsItem()) THEN
  self%Item => ItemValue
ELSE
  print*, "List Item Already Set"
END IF


END SUBROUTINE SetItem_PointerList

FUNCTION GetItem_AllocationList(self)

CLASS(Allocation_List), INTENT(IN)  :: self
CLASS(*),               ALLOCATABLE :: GetItem_AllocationList


IF(self%IsItem()) THEN
  ALLOCATE(GetItem_AllocationList, SOURCE=self%Item)
END IF  

END FUNCTION GetItem_AllocationList

FUNCTION GetItem_PointerList(self)

CLASS(Pointer_List), INTENT(IN)  :: self
CLASS(*),            POINTER :: GetItem_PointerList


IF(self%IsItem()) THEN
  GetItem_PointerList => self%Item
END IF  

END FUNCTION GetItem_PointerList

SUBROUTINE FreeItem_AllocationList(self)

CLASS(Allocation_List), INTENT(INOUT)  :: self


IF(self%IsItem()) DEALLOCATE(self%Item)

END SUBROUTINE FreeItem_AllocationList

SUBROUTINE FreeItem_PointerList(self)

CLASS(Pointer_List), INTENT(INOUT)  :: self


IF(self%IsItem()) NULLIFY(self%Item)

END SUBROUTINE FreeItem_PointerList

LOGICAL FUNCTION IsNext_AllocationList(self)

CLASS(Allocation_List), INTENT(IN) :: self


IsNext_AllocationList = ALLOCATED(self%Next)

END FUNCTION IsNext_AllocationList

LOGICAL FUNCTION IsNext_PointerList(self)

CLASS(Pointer_List), INTENT(IN) :: self


IsNext_PointerList = ASSOCIATED(self%Next)

END FUNCTION IsNext_PointerList

SUBROUTINE SetNext_AllocationList(self, NextValue)

CLASS(Allocation_List), INTENT(INOUT) :: self
CLASS(Allocation_List), INTENT(IN)    :: NextValue


ALLOCATE(self%Next, SOURCE=NextValue)

END SUBROUTINE SetNext_AllocationList

SUBROUTINE SetNext_PointerList(self, NextValue)

CLASS(Pointer_List),         INTENT(INOUT) :: self
CLASS(Pointer_List), TARGET, INTENT(IN)    :: NextValue


self%Next => NextValue

END SUBROUTINE SetNext_PointerList

FUNCTION GetNext_AllocationList(self)

CLASS(Allocation_List), TARGET, INTENT(IN)  :: self
CLASS(Allocation_List), POINTER             :: GetNext_AllocationList


IF(self%IsNext()) THEN
  GetNext_AllocationList => self%Next
END IF

END FUNCTION GetNext_AllocationList

FUNCTION GetNext_PointerList(self)

CLASS(Pointer_List), INTENT(IN)  :: self
CLASS(Pointer_List), POINTER     :: GetNext_PointerList


IF(self%IsNext()) THEN
  GetNext_PointerList => self%Next
ELSE
  NULLIFY(GetNext_PointerList)
END IF

END FUNCTION GetNext_PointerList

SUBROUTINE FreeNext_AllocationList(self)

CLASS(Allocation_List), INTENT(INOUT)  :: self


IF(self%IsNext()) DEALLOCATE(self%Next)

END SUBROUTINE FreeNext_AllocationList

SUBROUTINE FreeNext_PointerList(self)

CLASS(Pointer_List), INTENT(INOUT)  :: self


IF(self%IsNext()) NULLIFY(self%Next)

END SUBROUTINE FreeNext_PointerList

SUBROUTINE Reset_AllocationList(self)

CLASS(Allocation_List), INTENT(INOUT)  :: self


CALL self%FreeItem()
CALL self%FreeNext()

END SUBROUTINE Reset_AllocationList

SUBROUTINE Reset_PointerList(self)

CLASS(Pointer_List), INTENT(INOUT)  :: self


CALL self%FreeItem()
CALL self%FreeNext()

END SUBROUTINE Reset_PointerList

SUBROUTINE Display_AllocationListItem(self)

CLASS(Allocation_List), INTENT(IN) :: self


IF(self%IsItem()) THEN
  SELECT TYPE(ItemValue => self%GetItem())
    TYPE IS (LOGICAL)
      print*, ItemValue
    TYPE IS (CHARACTER(*))
      print*, ItemValue
    TYPE IS (INTEGER)
      print*, ItemValue
    TYPE IS (REAL)
      print*, ItemValue
    CLASS IS (Allocation_List)
      CALL ItemValue%Display()
    CLASS DEFAULT
      print*, "Unexpected Type of Item"
  END SELECT
ELSE
  print*, "List Item Not Set"
END IF

END SUBROUTINE Display_AllocationListItem

SUBROUTINE Display_PointerListItem(self)

CLASS(Pointer_List), INTENT(IN) :: self


IF(self%IsItem()) THEN
  SELECT TYPE(ItemValue => self%GetItem())
    TYPE IS (LOGICAL)
      print*, ItemValue
    TYPE IS (CHARACTER(*))
      print*, ItemValue
    TYPE IS (INTEGER)
      print*, ItemValue
    TYPE IS (REAL)
      print*, ItemValue  
    CLASS IS (Pointer_List)
      CALL ItemValue%Display()
    CLASS DEFAULT
      print*, "Unexpected Type of Item"
  END SELECT
ELSE
  print*, "List Item Not Set"
END IF

END SUBROUTINE Display_PointerListItem


END MODULE ListMod
