-- Ex1
data Nat = Zero | Succ Nat deriving (Show)

add' :: Nat -> Nat -> Nat
add' Zero n = n 
add' (Succ m) n = Succ (add' m n)

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add' m (mult m n)

-- Ex2

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = case compare x y of   
                          LT -> occurs x r
                          EQ -> True
                          GT -> occurs x l

-- Ex3
--
countLeaves :: Tree a -> Int
countLeaves (Leaf y) = 1 
countLeaves (Node l y r) = 1 + countLeaves l + countLeaves r

balanced :: Tree a -> Bool
balanced (Leaf y) = False
balanced (Node l x r) = leftLeaves == rightLeaves
                       where leftLeaves = countLeaves l
                             rightLeaves = countLeaves r
