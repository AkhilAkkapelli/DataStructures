PROGRAM test_PointerLinkedList
USE LinkedListMod
IMPLICIT NONE

INTEGER :: i=1
CHARACTER :: j = "a"
LOGICAL :: k = .TRUE.



TYPE(Pointer_List), TARGET ::  PL1, PL2, PL3

TYPE(Pointer_LinkedList) :: PLL1


CALL PL1%SetItem(i)
CALL PL2%SetItem(j)
CALL PL3%SetItem(k)

CALL PLL1%Initiate(PL1)
CALL PLL1%Connect(PL2)
CALL PLL1%Connect(PL3)

CALL PLL1%DispCurr()
CALL PLL1%ForCurr()
CALL PLL1%DispCurr()
CALL PLL1%ForCurr()
CALL PLL1%DispCurr()
CALL PLL1%ForCurr()

CALL PLL1%Display()


END PROGRAM test_PointerLinkedLIst
