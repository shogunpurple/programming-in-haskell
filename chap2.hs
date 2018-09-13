double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

{-
  ex2

  (2^3)*4
  (2*3)+(4*5)
  2+(3*(4^5)) 
-}

-- ex3
n = a `div` (length xs)
    where
      a = 10
      xs = [1,2,3,4,5]

-- ex4
lastagain xs = xs !! (length xs - 1)

-- ex5
initagain xs = reverse (tail (reverse xs))
initagains xs = take ((length xs) - 1) xs 
