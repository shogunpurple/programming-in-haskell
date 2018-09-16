-- Ex1
-- [Char]
-- (Char, Char, Char)
-- [(Bool, Char)]
-- ([Bool], [Char])
-- [[a] -> [a]]

-- Ex2
bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1], [2]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x 

-- Ex3 
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Int -> Int
double x = x*2

palindrome :: Ord a => a -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a  
twice f x = f (f x)
