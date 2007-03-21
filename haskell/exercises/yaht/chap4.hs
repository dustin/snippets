-- 4.4
data Triple a b c = Triple a b c

tripleFst (Triple a _ _) = a
tripleSnd (Triple _ b _) = b
tripleThr (Triple _ _ c) = c

-- 4.5
data Quadruple a b c d = Quadruple a a b b

firstTwo (Quadruple a b _ _) = [a, b]
lastTwo (Quadruple _ _ c d) = [c, d]

-- 4.6
data Tuple a b c d =
	  One a
	| Two (a, b)
	| Three (a, b, c)
	| Four (a, b, c, d)

tuple1 (One a) = Just a
tuple1 (Two (a, _)) = Just a
tuple1 (Three (a, _, _)) = Just a
tuple1 (Four (a, _, _, _)) = Just a

tuple2 (One a) = Nothing
tuple2 (Two (a, b)) = Just b
tuple2 (Three (a, b, _)) = Just b
tuple2 (Four (a, b, _, _)) = Just b

tuple3 (One a) = Nothing
tuple3 (Two (a, b)) = Nothing
tuple3 (Three (a, b, c)) = Just c
tuple3 (Four (a, b, c, _)) = Just c

tuple4 (One a) = Nothing
tuple4 (Two (a, b)) = Nothing
tuple4 (Three (a, b, c)) = Nothing
tuple4 (Four (a, b, c, d)) = Just d

-- 4.7
tuple_thing (One a) = Left (Left a)
tuple_thing (Two x) = Left (Right x)
tuple_thing (Three x) = Right (Left x)
tuple_thing (Four x) = Right (Right x)

-- 4.8 list stuff
data List a = Nil
	| Cons a (List a)

listHead Nil = Nothing
listHead (Cons a _) = Just a

listTail Nil = Nothing
listTail (Cons a Nil) = Just a
listTail (Cons _ x) = listTail x

listFoldl p i Nil = i
listFoldl p i (Cons a x) = listFoldl p (p i a) x

listFoldr p i Nil = i
listFoldr p i (Cons a x) = p a (listFoldr p i x)

-- 4.9 binary tree
data BinaryTree a
	= Leaf a
	| Branch (BinaryTree a) a (BinaryTree a)

elements (Leaf a) = [a]
elements (Branch l x r) =
	elements l ++ [x] ++ elements r
