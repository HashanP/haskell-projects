{-
Inspired by: http://5outh.blogspot.co.uk/2013/05/symbolic-calculus-in-haskell.html
-}

infixl 4 :+
infixl 5 :*, :/
infixl 6 :^

data Sym a
  = Const Int
  | Var Char
  | (Sym a) :+ (Sym a)
  | (Sym a) :* (Sym a)
  | (Sym a) :/ (Sym a)
  | (Sym a) :^ (Sym a)
  deriving (Show, Eq)

diff :: Char -> Sym a -> Sym a
diff x (Const _) = Const 0

diff x (Var y)
  | x == y = Const 1
  | x /= y = Var y

-- Chain rule
diff z (x :^ y) = (y :* (x :^ (y :+ (Const (-1))))) :* (diff z x)

-- Product rule
diff z (u :* v) = (u :* (diff z v)) :+ (v :* (diff z u))

-- Quotient rule
diff z (u :/ v) = ((v :* (diff z u)) :+ ((Const (-1)) :* u :* (diff z v)) :/ (v :^ (Const 2)))

-- Distributive over addition (linear operator)
diff z (x :+ y) = (diff z x) :+ (diff z y)

simplify ((Const x) :+ (Const y)) = Const (x + y)
simplify ((Const 1) :* x) = (simplify x) 
simplify (x :* (Const 1)) = (simplify x) 
simplify (x :^ (Const 1)) = (simplify x) 
simplify (x :^ y) = (simplify x) :^ (simplify y)
simplify (x :* y) = (simplify x) :* (simplify y)
simplify x = x

main = do
  print $ simplify $ diff 'x' ((Var 'x') :^ (Const 2))
