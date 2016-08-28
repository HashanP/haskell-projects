import Data.List (intersperse)

data Polynomial a = Polynomial [a]

zipWithIndex :: (Integral b) => [a] -> b -> [(a,b)]
zipWithIndex [] _ = []
zipWithIndex (x:xs) n = (x,n):zipWithIndex xs (n-1)

instance (Num a, Eq a, Show a) => Show (Polynomial a) where
  show (Polynomial xs) = foldl1 (++) . intersperse " + " . filter (not . null) . map wrap $ zipWithIndex xs ((length xs) - 1)
    where wrap (0, _) = ""
          wrap (1, 1) = "x"
          wrap (coefficient, 1) = show coefficient ++ "x"
          wrap (coefficient, 0) = show coefficient
          wrap (1, exponent) = "x^" ++ show exponent
          wrap (coefficient, exponent) = show coefficient ++ "x^" ++ show exponent

degree (Polynomial xs) = length xs

ensureAtLeast xs n
  | length xs < n = ensureAtLeast (0:xs) n
  | otherwise = xs

add (Polynomial xs) (Polynomial ys) = Polynomial $ zipWith (+) (ensureAtLeast xs maxDegree) (ensureAtLeast ys maxDegree)
  where maxDegree = max (length xs) (length ys)

sub (Polynomial xs) (Polynomial ys) = Polynomial $ zipWith (-) (ensureAtLeast xs maxDegree) (ensureAtLeast ys maxDegree)
  where maxDegree = max (length xs) (length ys)

differentiate :: (Num a) => Polynomial a -> Polynomial a
differentiate (Polynomial []) = Polynomial []
differentiate (Polynomial xs) = Polynomial . init . map diffTerm $ zipWithIndex xs ((length xs) - 1)
  where diffTerm (coefficient, exponent) = coefficient * (fromIntegral exponent)

evaluate :: (Num a) => a -> Polynomial a -> [a]
evaluate x (Polynomial xs) = scanl1 (\acc coefficient -> (acc * x) + coefficient) xs

main = do
  print $ evaluate 4 $ differentiate (Polynomial [5, 3, 0, 2])
