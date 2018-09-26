-- Ex1
fac :: Int -> Int
fac 0 = 1
fac n | n > 0 = n * fac (n-1)
      | otherwise = 1

-- Ex2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- Ex3
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m Main.^ (n - 1))

{-
  2 ^ 3
  applying ^
  2 * (2 ^ 2)
  applying ^
  2 * 2 * (2 ^ 1)
  applying ^
  2 * 2 * 2 * (2 ^ 0)
  2 * 2 * (2 * 1)
  2 * 2 * 2 * 1 = 8
-}

-- Ex4
euclid :: Int -> Int -> Int
euclid 0 0 = 0
euclid m n | m > n = euclid (m - n) n
           | m < n = euclid (n - m) m
           | m == n = m

-- Ex5
length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs
{-
   length:
   length [1,2,3]
   applying length
   1 + length [2,3]
   applying length
   1 + 1 + length [3]
   applying length
   1 + 1 + 1 + length []
   applying length
   1 + 1 + 1 + 0
   3
-} 

drop' :: Int -> [a] -> [a]
drop' 0 xs     = xs
drop' _ []     = []
drop' n (_:xs) = drop' (n - 1) xs
{-
   drop:
   drop 3 [1,2,3,4]
   applying drop
   drop 2 [2,3,4]
   applying drop
   drop 1 [3,4]
   applying drop
   drop 0 [4]
   applying drop
   [4]
-} 

init' :: [a] -> [a]
init' [_]    = []
init' (x:xs) = x : init' xs
{-
   init:
   init [1,2,3,4]
   applying init
   1 + (init [2,3,4])
   applying init
   1 : 2 : (init [3,4])
   applying init
   1 : 2 : 3 : (init [4])
   applying init
   1 : 2 : 3 : []
   [1,2,3]
-} 

-- Ex6
-- a
and' :: [Bool] -> Bool
and' [] = False
and' [a] = a
and' (x:xs) | x == True = True
            | otherwise = and' xs
-- b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- c
replicate' :: Int -> a -> [a]
replicate' 0 a = [] 
replicate' n a = a : replicate' (n-1) a 

-- d
(<!!>) :: [a] -> Int -> a
(<!!>) (x:xs) 0 = x
(<!!>) (x:xs) n = (<!!>) xs (n-1)

-- e
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = if x == a then True else elem' a xs 

-- Ex7
merge' :: Ord a => [a] -> [a] -> [a]
merge' xs [] = xs
merge' [] ys = ys 
merge' (x:xs) (y:ys) | x < y = x : merge' xs (y:ys)
                     | otherwise = y : merge' (x:xs) ys 
{-
   merge:
   merge [2,5,6] [1,3,4]
   applying merge
   1 : merge [2,5,6] [3,4] 
   applying merge
   1 : 2 : merge [5,6] [3,4] 
   applying merge
   1 : 2 : 3: merge [5,6] [4] 
   applying merge
   1 : 2 : 3 : 4 : merge [5,6] [] 
   applying merge
   1 : 2 : 3 : 4 : [5,6]
   [1,2,3,4,5,6]
-}

-- Ex8
-- merge sort
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take n xs, (drop n xs))
  where n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge' (msort first) (msort second)
  where (first, second) = halve xs

-- Ex9
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take (n-1) xs

last' :: [a] -> a
last' [a] = a
last' (x:xs) = last xs 
