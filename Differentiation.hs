{-
Inspired by: http://5outh.blogspot.co.uk/2013/05/symbolic-calculus-in-haskell.html
-}
import Data.List (intersperse)

data Sym a
  = Var Char
  | Sum [Sym a] Int
  | Product [Sym a]
  | Exponentation (Sym a) (Sym a)
  deriving (Show, Eq)

simplify :: Sym a -> Sym a

simplify (Sum [x] 0) = simplify x

simplify (Sum xs const) = Sum (invalid ++ extractTerms) (const + extractConst)
  where pred (Sum _ _) = True
        pred _ = False
        valid = filter pred subCleaned
        invalid = filter (not . pred) subCleaned
        extractTerms = concat $ map (\(Sum xs const) -> xs) valid
        extractConst = foldl (+) 0 $ map (\(Sum xs const) -> const) valid
        subCleaned = map simplify xs

simplify (Product [x]) = simplify x

simplify (Product xs) = Product (getNotConst ++ extractTerms ++ memy2)
  where pred (Product _) = True
        pred _ = False
        valid = filter pred subCleaned
        invalid = filter (not . pred) subCleaned
        extractTerms = concat $ map (\(Product xs) -> xs) valid
        subCleaned = map simplify xs
        getConst = filter pred2 invalid
        getNotConst = filter (not . pred2) invalid
        pred2 (Sum [] _) = True
        pred2 _ = False
        mapsy (Sum [] n) = n
        memy = foldl (*) 1 $ map mapsy getConst
        memy2
          | memy == 1 = []
          | otherwise = [Sum [] memy]

simplify (Exponentation x y)
  | y' == (Sum [] 1) = x'
  | otherwise = (Exponentation x' y')
  where x' = simplify x
        y' = simplify y

simplify x = x

diff :: Char -> Sym a -> Sym a

-- Differentiating variables
diff x (Var y)
  | x == y = (Sum [] 1)
  | otherwise = (Sum [] 0)

-- Power rule -- Doesn't work yet if you have a funciton of x as the exponent
diff x (Exponentation base exponent) = Product [(Exponentation base (Sum [exponent] (-1))), exponent]

-- Distributive over addition (Linear operator)
diff x (Sum xs _) = Sum (map (diff x) xs) 0

-- Product rule
diff z (Product xs) = Product [Sum (map term [0..(length xs - 1)]) 0]
  where term x = Product ((take x xs) ++ [diff z (xs !! x)] ++ (drop (x+1) xs))

pretty :: Sym a -> String

pretty (Var x) = [x]

pretty (Sum xs x)
  | m == [] && x == 0 = ""
  | m == [] = (show x)
  | x == 0 = (concat $ intersperse " + " m)
  | otherwise = (show x) ++ " + " ++ (concat $ intersperse " + " m)
  where m = map pretty xs

pretty (Exponentation x y) = "(" ++ (pretty x) ++ ") ^ (" ++ (pretty y) ++ ")"

pretty (Product xs) = "(" ++ (concat $ intersperse ")(" (map pretty xs)) ++ ")"
