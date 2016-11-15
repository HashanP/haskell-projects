{-
Inspired by: http://5outh.blogspot.co.uk/2013/05/symbolic-calculus-in-haskell.html
-}
import Data.List (intersperse)

{-
The way I store expressions is a bit weird. Originally I had a Const data constructor as part of my
Sym type. The problem was I didn't like having to filter out and simplify multiple Consts in a single
summation, for example. Or multiple Consts in a single product. So I made the Sum data constructor.
The list property of the Sum data constructor stores summands/addends that are symbolic expressions,
the integer stored the accumulated constant addend. 
-}

data Sym a
  = Var Char -- Variables
  | Sum [Sym a] Int -- Additions
  | Product [Sym a] -- Products
  | NatLog (Sym a) -- Natural logarithm
  | Exponentation (Sym a) (Sym a) -- Exponentation
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

--simplify (Product [x]) = simplify x

simplify (Product xs)
  | (Sum [] 0) `elem` combined = Sum [] 0
  | length combined == 1 = head combined
  | otherwise = Product combined
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
        combined = getNotConst ++ extractTerms ++ memy2

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

-- Exponentation
diff x entire@(Exponentation base exponent) = Sum [part1, part2] 0
  where part1 = Product [diff x exponent, (NatLog base), entire]
        part2 = Product [exponent, diff x (NatLog base), entire]

-- Distributive over addition (Linear operator)
diff x (Sum xs _) = Sum (map (diff x) xs) 0

diff x (NatLog y) = Product [diff x y, Exponentation y (Sum [] (-1))]

-- Product rule
diff z (Product xs) = Product [Sum (map term [0..(length xs - 1)]) 0]
  where term x = Product ((take x xs) ++ [diff z (xs !! x)] ++ (drop (x+1) xs))

pretty :: Sym a -> String

pretty (Var x) = [x]

pretty (NatLog x) = "ln(" ++ (pretty x) ++ ")"

pretty (Sum xs x)
  | m == [] && x == 0 = ""
  | m == [] = (show x)
  | x == 0 = (concat $ intersperse " + " m)
  | otherwise = (show x) ++ " + " ++ (concat $ intersperse " + " m)
  where m = map pretty xs

pretty (Exponentation x y) = "(" ++ (pretty x) ++ ") ^ (" ++ (pretty y) ++ ")"

pretty (Product xs) = "(" ++ (concat $ intersperse ")(" (map pretty xs)) ++ ")"

main = do
  print $ pretty $ simplify $ diff 'x' 
    $ Exponentation (Var 'x') (Sum [] 2)

  print $ pretty $ simplify $ diff 'x' 
    $ NatLog (Var 'x')

  print $ pretty $ simplify $ diff 'x' 
    $ Exponentation (Var 'x') (Var 'x')

  print $ pretty $ simplify $ diff 'x' 
    $ NatLog (Exponentation (Var 'x') (Sum [] 2))

  print $ pretty $ simplify $ diff 'x' 
    $ Product [
      Exponentation (Var 'x') (Sum [] 2),
      Exponentation (Sum [Var 'x'] 2) (Sum [] 5)
      ]
