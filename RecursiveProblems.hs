{-
Implementation of Russian peasant multiplication
-}

multiply :: (Integral a) => a -> a -> a
multiply 0 _ = 0
multiply x y 
  | x `mod` 2 == 1 = y + rest
  | otherwise = rest
  where rest = multiply (x `div` 2) (y * 2)

{- 
Tower of Hanoi
-}

hanoi 0 = 0
hanoi n = (2 * hanoi (n - 1)) + 1

{-
Exponentation
-}

pow :: (Fractional a) => a -> Int -> a
pow a 1 = a
pow a (-1) = 1 / a
pow a b
  | b `mod` 2 == 0 = pow (a * a) (b `div` 2)
  | otherwise = a * (pow (a * a) (b `div` 2))

