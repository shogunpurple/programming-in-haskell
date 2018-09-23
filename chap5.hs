-- Chapter 5 - List comprehensions
-- Ex1
firstOneHundredIntSquares :: Int
firstOneHundredIntSquares = sum [x^2 | x <- [1..100]] 

-- Ex2
grid :: Int -> Int -> [(Int,Int)]
grid x y = [(x,y) | x <- [0..x], y <- [0..y]]

-- Ex3 - unsure
{-square :: Int -> [(Int, Int)]-}
{-square n = [grid x | x <- [0..n]]-}

-- Ex4
replicate' :: Int -> a -> [a] 
replicate' n val =  [val | x <- [1..n]]

-- Ex5 - unsure 
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [0..], y <- [0..], z <- [0..], x+y+z < n]

-- Ex6
-- A perfect integer is one that equals the sum of all its factors. Excluding the number itself.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [0..n-1], sum (factors x) - x == x] 

-- Ex7
-- Show how the list comprehension [(x, y) | x <- [1, 2], y <- [3, 4]] with two generators can be
-- expressed using two comprehensions with a single generator
nestedComprehension xs ys = concat [[(x, y) | y <- ys] | x <- xs]

-- Ex8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

findPositions :: Eq a => a -> [a] -> [Int]
findPositions x xs = find x (zip xs [0..])

-- Ex9
-- The scalar product of 2 lists equals the sum of the products of corresponding integers.
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x'*y | (x',y) <- zip xs ys]

