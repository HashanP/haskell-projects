-- Magnitude
magnitude :: (Floating a, Num a) => [a] -> a
magnitude = sqrt . sum . map (** 2)

-- Dot Product
dot :: (Floating a, Num a) => [a] -> [a] -> a
dot [] [] = 0
dot (x: xs) (y: ys) = (x * y) + (dot xs ys)

-- Angle
angle :: (Floating a, Num a) => [a] -> [a] -> a
angle a b = acos $ (dot a b) / ((magnitude a) * (magnitude b))
