solutions :: [[Int]]
solutions = getSolutions 8 [[]]
  
getSolutions :: Int -> [[Int]] -> [[Int]]
getSolutions 0 = id
getSolutions n = getSolutions (n-1) . filter invalid . concat . map addRow

addRow :: [Int] -> [[Int]]
addRow solution = map (\x -> solution ++ [x]) . filter (`notElem` solution) $ [0..7]

invalid :: [Int] -> Bool
invalid solution = not $ any (containsInvalid solution) [0..((length solution) - 1)]

containsInvalid :: [Int] -> Int -> Bool
containsInvalid solution n = any (diagonalThreaten solution n) [1..((length solution - 1) - n)]

diagonalThreaten :: [Int] -> Int -> Int -> Bool
diagonalThreaten solution n x = (solution !! n) + x == (solution !! (n+x)) || (solution !! n) -x == (solution !! (n+x))
