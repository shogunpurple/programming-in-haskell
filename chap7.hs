-- Ex1
listComp :: Integral a => (a -> b) -> (a -> Bool) -> [a] -> [b]
listComp f p = map f . filter p

-- Ex2
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) | p x = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs


-- Ex3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x:xs else xs) [] 

-- Ex4
dec2int :: [Int] -> Int
dec2int xs = read (foldl (\acc x -> acc ++ (show x)) "" xs)

-- Ex5 
currry :: ((a , b) -> c) -> a -> b -> c
currry f = \x y -> f (x, y)

uncurrry :: (a -> b -> c) -> ((a, b) -> c) 
uncurrry f = \(x,y) -> f x y

-- Ex6

