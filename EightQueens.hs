solutions :: [[Int]]
solutions = getSolutions 8 8 [[]]
  
getSolutions :: Int -> Int -> [[Int]] -> [[Int]]
getSolutions 0 m = id
getSolutions n m = getSolutions (n-1) m . filter invalid . concat . map (addRow m)

addRow :: Int -> [Int] -> [[Int]]
addRow m solution = map (\x -> solution ++ [x]) . filter (`notElem` solution) $ [0..(m-1)]

invalid :: [Int] -> Bool
invalid solution = not $ any (containsInvalid solution) [0..((length solution) - 1)]

containsInvalid :: [Int] -> Int -> Bool
containsInvalid solution n = any (diagonalThreaten solution n) [1..((length solution - 1) - n)]

diagonalThreaten :: [Int] -> Int -> Int -> Bool
diagonalThreaten solution n x = (solution !! n) + x == (solution !! (n+x)) || (solution !! n) -x == (solution !! (n+x))
