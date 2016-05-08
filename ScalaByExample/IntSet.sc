

/*val set = EmptySet.incl(3).incl(1).incl(5)
val set2 = EmptySet.incl(8).incl(6).incl(4)
val set3 = EmptySet.incl(8).incl(1)
val set4 = set.union(set2)
val set5 = set.intersection(set2)
val set6 = set.union(set2).intersection(set3)
val set7 = set4.excl(5).toString

val one = ZeroInt.successor
val two = one.successor
val three = two.successor
(two+three)
(ZeroInt+two)
(three-ZeroInt)
(three-two)
(three-three)
(ZeroInt-one)
(ZeroInt-two)
(ZeroInt-three)
(ZeroInt-three+two-one+two)
val minusThree = (ZeroInt-three)
minusThree+two
three+minusThree
three-minusThree
minusThree-minusThree
minusThree+minusThree*/

val s = EmptyStack
val w = s push(1) push(2) push(3)
w.top
w.pop.top
w.pop.pop.top
w.pop.pop.pop isEmpty

