type Church a = (a -> a) -> a -> a

zero :: Church a
zero = \f -> \x -> x

succ' :: (Church a) -> (Church a)
succ' church = \f -> \x -> f (church f x)

toNum :: (Num a) => (Church a) -> a
toNum church = church (+1) 0

add :: (Church a) -> (Church a) -> (Church a)
add m n = \f -> \x -> m f (n f x)

mul :: (Church a) -> (Church a) -> (Church a)
mul m n = \f -> \x -> m (n f) x
