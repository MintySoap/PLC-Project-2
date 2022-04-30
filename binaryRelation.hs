import Distribution.Simple.Setup (trueArg)
type Element = (Int,Int)
type BinaryRelation = Element -> Bool
bound = 99

-- returns an empty binary relation, i.e. no pairs
emptyBinaryRelation :: BinaryRelation
emptyBinaryRelation = \x -> False


-- returns the universal binary relation, i.e. all pairs
universalBinaryRelation :: BinaryRelation
universalBinaryRelation = \x -> True


-- returns True if element is in binary relation; False otherwise
contains :: Element -> BinaryRelation -> Bool
contains e r = r e


-- returns a new binary relation obtained by adding element to binary relation; 
-- if element is already in binary relation then returns same binary relation
add :: Element -> BinaryRelation -> BinaryRelation
add e r = \x -> if e == x then True else r x


-- returns new binary relation obtained by adding all elements in input list to
-- given binary relation.
addMultiple :: [Element] -> BinaryRelation -> BinaryRelation
addMultiple es r = foldr add r es


-- returns True if binary relation 1 is a subset of binary relation 2;
-- False otherwise.
subRelation :: BinaryRelation -> BinaryRelation -> Bool
subRelation r1 r2 = subRelationHelper1 r1 r2 0

--recursive helper function that basically cycles through the first number (n,m)
subRelationHelper1 :: BinaryRelation -> BinaryRelation -> Int -> Bool
subRelationHelper1 r1 r2 n | n > bound = True --termination condition
                           | otherwise = subRelationHelper2 r1 r2 n 0 && subRelationHelper1 r1 r2 (n+1) --cycles through n

--cycles through m so if n is 1 then it cycles through (1,1), (1,2), ...
--checks if the element of r1 is in r2
subRelationHelper2 :: BinaryRelation -> BinaryRelation -> Int -> Int-> Bool
subRelationHelper2 r1 r2 n m | m > bound = True --termination condition
                             | contains (n,m) r1 && not (contains (n,m) r2) = False
                             | otherwise = subRelationHelper2 r1 r2 n (m+1) --cycles through m



-- returns True if binary relation 1 and binary relation 2 are equal;
-- False otherwise.
equal :: BinaryRelation -> BinaryRelation -> Bool
equal r1 r2 = equalHelper1 r1 r2 0

equalHelper1 :: BinaryRelation -> BinaryRelation -> Int -> Bool 
equalHelper1 r1 r2 n | n > bound = True 
                     | otherwise = equalHelper2 r1 r2 n 0 && equalHelper1 r1 r2 (n+1)

equalHelper2 :: BinaryRelation -> BinaryRelation -> Int -> Int -> Bool 
equalHelper2 r1 r2 n m | m > bound = True 
                       | not (contains (n,m) r1) && contains (n,m) r2 = False --when one of them has it and the other doesn't then they are not matching
                       | contains (n,m) r1 && not(contains (n,m) r2) = False
                       | otherwise = equalHelper2 r1 r2 n (m+1)



-- for all a, (a,a) is in binary relation
reflexive :: BinaryRelation -> Bool
reflexive r = foldr (\x acc -> r (x,x) && acc) True [0..bound]


-- returns the union of the two input binary relations
union :: BinaryRelation -> BinaryRelation -> BinaryRelation
union r1 r2 = addMultiple [(x,y) | x <- [0..bound], y <- [0..bound], contains (x,y) r2] r1


-- returns inverse of input binary relation; 
-- inverse(r) = { (b,a) | (a,b) in r }
inverse :: BinaryRelation -> BinaryRelation
inverse r = addMultiple [(x,y) | x <- [0..99], y <- [0..99], contains (y,x) r] emptyBinaryRelation --Basically just adds the inverses of the binary relation to an empty one


-- returns True if input binary relation is symmetric; False otherwise
symmetric :: BinaryRelation -> Bool
symmetric r = symmetricHelper1 r 0

--same idea as equal helpers
symmetricHelper1 :: BinaryRelation -> Int -> Bool 
symmetricHelper1 r n | n > bound = True 
                     | otherwise = symmetricHelper2 r n 0 && symmetricHelper1 r (n+1)

symmetricHelper2 :: BinaryRelation -> Int -> Int -> Bool 
symmetricHelper2 r n m | m > bound = True 
                       | contains (n,m) r && not (contains (m,n) r) = False  -- de morgan's law. if r1 contains both (n,m) and (m,n) then true. If not then false
                       | not (contains (n,m) r) && contains (m,n) r = False
                       | otherwise = symmetricHelper2 r n (m+1)



-- note: fixed but needs testing
-- returns True if input binary relation is anti-symmetric; False otherwise
antiSymmetric :: BinaryRelation -> Bool
antiSymmetric r = antiSymmetricHelper1 r 0

--same idea as equal helpers
antiSymmetricHelper1 :: BinaryRelation -> Int -> Bool 
antiSymmetricHelper1 r n | n > bound = True 
                         | otherwise = antiSymmetricHelper2 r n 0 && antiSymmetricHelper1 r (n+1)

antiSymmetricHelper2 :: BinaryRelation -> Int -> Int -> Bool 
antiSymmetricHelper2 r n m | m > bound = True 
                           | contains (n,m) r && contains (m,n) r && n /= m = False
                           | otherwise = antiSymmetricHelper2 r n (m+1)



-- returns True if input binary relation is transitive; False otherwise
--nested foldr statements for a,b,c
--note: fixed but needs testing
transitive :: BinaryRelation -> Bool
transitive r = transitiveHelper1 r 0

transitiveHelper1 :: BinaryRelation -> Int -> Bool 
transitiveHelper1 r a | a > bound = True 
                      | otherwise = transitiveHelper2 r a 0 && transitiveHelper1 r (a+1)

transitiveHelper2 :: BinaryRelation -> Int -> Int -> Bool 
transitiveHelper2 r a b | b > bound = True 
                        | otherwise = transitiveHelper3 r a b 0 && transitiveHelper2 r a (b+1)

transitiveHelper3 :: BinaryRelation -> Int -> Int -> Int -> Bool 
transitiveHelper3 r a b c | c > bound = True 
                          | (contains (a,b) r && contains (b,c) r) && not(contains (a,c) r)= False
                          | otherwise = transitiveHelper3 r a b (c+1)



-- returns True if input binary relation is an equivalence; False otherwise
equivalence :: BinaryRelation -> Bool
equivalence r = reflexive r && symmetric r && transitive r



-- look up definition in BinaryRelationNotes.pdf
-- basically I need to turn a Closure into a reflexiveClosure
--note: needs testing
reflexiveClosure :: BinaryRelation -> BinaryRelation
reflexiveClosure r | reflexive r = r
                   | otherwise = addMultiple [(x,x) | x <- [0..bound]] r


-- look up definition in BinaryRelationNotes.pdf
--note: needs testing
symmetricClosure :: BinaryRelation -> BinaryRelation
symmetricClosure r | symmetric r = r
                   | otherwise = addMultiple [(b,a) | b <- [0..bound], a <- [0..bound], r (a,b)] r


-- selfJoin will be useful in implementing transitiveClosure;
-- returns selfJoin(r) = { (a,c) | (a,b) is in r and (b,c) is in r }
--note: needs testing
selfJoin :: BinaryRelation -> BinaryRelation
selfJoin r = selfJoinHelper r emptyBinaryRelation 0

selfJoinHelper :: BinaryRelation -> BinaryRelation -> Int -> BinaryRelation
selfJoinHelper r new_r b | b > bound = new_r
                         | otherwise = selfJoinHelper r (addMultiple [(a,c) | a <- [0..bound], c <- [0..bound], contains (a,b) r, contains (b,c) r] new_r) (b+1)


--note: needs testing
-- look up definition in BinaryRelationNotes.pdf
transitiveClosure :: BinaryRelation -> BinaryRelation
transitiveClosure r | transitive r = r
                    | otherwise = transitiveClosure (union (selfJoin r) r)

-- returns String version of binary relation
toString :: BinaryRelation -> String
toString r = show [(x,y) | x <- [0..bound], y <- [0..bound], r (x,y)]