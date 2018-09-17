halve :: [a] -> ([a], [a]) 
halve xs = (take half xs, drop half xs)
  where half = (length xs) `div` 2 

-- Ex2 IDK
third :: [a] -> a
third xs = head (tail (tail xs))
third xs = xs !! 2
third (_:_:x:_) = x

-- Ex3
-- Conditional
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs 

-- Guard Equations
safetail2 :: [a] -> [a]
safetail2 xs | null xs = [] 
  | otherwise = tail xs

-- Pattern matching
safetail3 :: [a] -> [a]
safetail3 [] = [] 
safetail3 xs = tail xs

-- Ex4
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

{-(||) :: Bool -> Bool -> Bool-}
{-False || False = False-}
{-_ || _ = True-}

{-(||) :: Bool -> Bool -> Bool-}
{-b || b = b-}
{-_ || _ = True-}

-- Ex5
func :: Bool -> Bool -> Bool
func a b = if a then
               if b then True else False
           else False

-- Ex6 
func' :: Bool -> Bool -> Bool
func' a b = if a then b else False


-- Ex7
mult :: Int -> Int -> Int -> Int
{-mult x y z = x*y*z-}
mult = \x -> (\y -> (\z -> x*y*z))

-- Ex8
luhnDouble :: Int -> Int
luhnDouble x = if doubled > 9 then doubled - 9 else doubled 
  where doubled = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = ((luhnDouble w) + x + (luhnDouble y) + z) `mod` 10 == 0
