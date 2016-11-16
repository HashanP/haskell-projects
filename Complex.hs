data Complex a 
  = Complex {
    getReal :: a,
    getImag :: a
  } deriving (Eq)

instance (Show a) => Show (Complex a) where
  show (Complex a b) = (show a) ++ " + " ++ (show b) ++ "i"

instance (Num a) => Num (Complex a) where
  (Complex a b) + (Complex c d) = Complex (a + c) (b + d)
  -- Gauss' method for multiplying complex number
  (Complex a b) * (Complex c d) = Complex (ac - bd) other
    where ac = a * c
          bd = b * d
          other = ac + bd - ((a - b) * (c - d))
  abs (Complex a b) = Complex (abs a) (abs b)
  signum (Complex a b) = Complex (signum a) (signum b)
  negate (Complex a b) = Complex (negate a) (negate b)
  fromInteger x = Complex (fromInteger x) 0

magnitude :: (Floating a) => (Complex a) -> a
magnitude (Complex a b) = sqrt ((a ** 2) + (b ** 2))
