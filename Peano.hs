data Peano = Zero | Succ Peano deriving (Show)

add :: Peano -> Peano -> Peano
add Zero x = x
add (Succ x) y = Succ (add x y)

sub :: Peano -> Peano -> Peano
sub x Zero = x
sub Zero _ = error "Cannot represent negative numbers with Peano"
sub (Succ x) (Succ y) = sub x y

mul :: Peano -> Peano -> Peano
mul _ Zero = Zero
mul Zero _ = Zero
mul (Succ x) y = add (mul x y) y

instance Eq Peano where
  Zero == Zero = True
  Zero == _ = False
  _ == Zero = False
  (Succ x) == (Succ y) = x == y

instance Ord Peano where
  Zero `compare` Zero = EQ
  Zero `compare` _ = LT
  _ `compare` Zero = GT
  (Succ x) `compare` (Succ y) = x `compare` y

infinity :: Peano
infinity = Succ infinity
