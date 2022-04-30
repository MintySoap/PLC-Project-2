# PLC-Project-2
Instructions taken from: 
https://tinman.cs.gsu.edu/~raj/4330/sp22/project2/


You will complete various functions in the following implementation of the "Binary Relation" data structure.

type Element = (Int,Int)
type BinaryRelation = Element -> Bool
bound = 99
BinaryRelation is set of pairs of integers, each integer is in the range 0 to bound. The choice of 99 as the value of bound is an arbitrary choice. Some examples of binary relations are shown below:

  r1 = { (1,5), (2,8), (3,9) }
  r2 = { (1,2), (2,3), (3,4), (4,5) }
The above notation is for human consumption. Internally, in our implementation, a BinaryRelation is defined as a function from (Int,Int) to Boolean, which indicates whether a given pair of integers is in the binary relation or not.
The binary relation r1 above can be represented by the function that maps (1,5) to True, (2,8) to True, (3,9) to True, and every other pair to False.

Similarly, binary relation r2 can be represented by the function that maps (1,2) to True, (2,3) to True, (3,4) to True, (4,5) to True, and every other pair to False.

Test cases:
r1 = add (1,2) (add (2,3) (add (3,4) emptyBinaryRelation))
putStrLn (toString r1)
r2 = addMultiple [(1,2),(2,3),(3,4),(4,5)] emptyBinaryRelation
putStrLn (toString r2)
r3 = addMultiple [(1,2),(2,3),(3,4),(4,5),(5,6)] emptyBinaryRelation
subRelation r2 r3
subRelation r3 r2
equal r2 r2
equal r2 r3
r4 = addMultiple [(n,n) | n <- [0..bound]] emptyBinaryRelation
reflexive r3
reflexive r4
putStrLn (toString r1)
putStrLn (toString r2)
putStrLn (toString (r1 `union` r2))
putStrLn (toString r1)
putStrLn (toString r2)
putStrLn (toString (inverse r1))
putStrLn (toString (inverse r2))
putStrLn (toString r1)
symmetric r1
symmetric (addMultiple [(2,1),(3,2),(4,3)] r1)
putStrLn (toString r1)
antiSymmetric r1
antiSymmetric (add (3,2) r1)
putStrLn (toString r1)
transitive r1
transitive (addMultiple [(1,3),(2,4),(1,4)] r1)
putStrLn (toString (reflexiveClosure r1))
putStrLn (toString r1)
putStrLn (toString (symmetricClosure r1))
putStrLn (toString r1)
putStrLn (toString (selfJoin r1))
putStrLn (toString r1)
putStrLn (toString (transitiveClosure r1))
