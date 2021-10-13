PROGRAM test_AllocationLinkedList
USE LinkedListMod
IMPLICIT NONE


INTEGER :: i=1
CHARACTER :: j = "a"
LOGICAL :: k = .TRUE.


TYPE(Allocation_List) ::  AL1, AL2, AL3

TYPE(Allocation_LinkedList) :: ALL1


CALL AL1%SetItem(i)
CALL AL2%SetItem(j)
CALL AL3%SetItem(k)

CALL ALL1%Initiate(AL1)
CALL ALL1%Connect(AL2)
CALL ALL1%Connect(AL3)

CALL ALL1%DispCurr()
CALL ALL1%ForCurr()
CALL ALL1%DispCurr()
CALL ALL1%ForCurr()
CALL ALL1%DispCurr()
CALL ALL1%ForCurr()

CALL ALL1%Display()


END PROGRAM test_AllocationLinkedList
