-- Implementation of primitive recursive functions in Haskell
-- http://web.archive.org/web/20060713073640/http://www.dcs.shef.ac.uk/~matt/teaching/04/com2030/lectures/tomlect13.pdf

data Prim 
  = Zero
  | Succ
  | Proj Int Int -- Index to extract (0-indexed), Length of tuple
  | Comp Prim [Prim]
  | Recursion Prim Prim

eval :: (Integral a, Eq a) => Prim -> [a] -> a
eval (Zero) _ = 0
eval (Succ) (x:xs) = x + 1
eval (Proj x _) xs = xs !! x
eval (Comp x prims) xs = eval x (map (flip eval xs) prims)
eval l@(Recursion base inductive) xs
  | last xs == 0 = eval base (init xs)
  | otherwise = eval inductive (init xs ++ [(last xs)-1, eval l (init xs ++ [(last xs)-1])])

toPrim :: Int -> Prim
toPrim 0 = Zero
toPrim x = Comp Succ [toPrim (x-1)]

add :: Prim
add = Recursion (Proj 0 1) (Comp Succ [Proj 2 3])

mul :: Prim
mul = Recursion (Zero) (Comp add [Proj 0 3, Proj 2 3])
