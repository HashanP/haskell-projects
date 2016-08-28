pair x y f = f x y
first x = x (\fst snd -> fst)
second x = x (\fst snd -> snd)

-- Convert Church Pair to Haskell Pair
toPair x = x (\fst snd -> (fst, snd))
