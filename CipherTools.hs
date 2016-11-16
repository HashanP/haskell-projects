import Data.Char (toUpper, isAlpha, ord, chr)
import Data.Function (on)
import Data.List (sortBy, intersperse, nub)
import Control.Monad (forM_)

freq :: String -> [(Char, Int)]
freq input = reverse $ sortBy (compare `on` snd) $ map addOccurences  ['A'..'Z']
  where str = map toUpper input
        addOccurences c = (c, length (filter (==c) str))

-- To use this, just load it into GHCI
-- And run: freq "fdsfdsfd"

freqTable :: String -> IO ()
freqTable input = mapM_ putStrLn $ map toStr frequencies
  where frequencies = freq input
        toStr (c, len) = [c] ++ ": " ++ (show len)

-- To use this, just load it into GHCI
-- And run: freqTable "fdsfdsfd"

grams :: String -> Int -> [(String, Int)]
grams input n = reverse $ sortBy (compare `on` snd) $ map addOccurences unique
  where convert l@(x:xs)
          | length l < n = []
          | otherwise = (take n l):convert xs
        conversion = convert (filter isAlpha input)
        unique = nub conversion
        addOccurences str = (str, length (filter (==str) conversion))

-- To use this, just load it into GHCI
-- And run: grams "fdsfdsfd" 2

gramsTable :: String -> Int -> IO ()
gramsTable input n = mapM_ putStrLn $ map toStr frequencies
  where frequencies = grams input n
        toStr (c, len) = c ++ ": " ++ (show len)

-- To use this, just load it into GHCI
-- And run: gramsTable "fdsfdsfd" 2

english :: [Float]
english = [
  0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406,
  0.06749, 0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074]

caesarCrack :: String -> [(Int, Float)]
caesarCrack input = sortBy (compare `on` snd) $ map rank [0..25]
  where str = map toUpper input
        rank n = (n, abs $ (convert $ transform str (26-n)))
        convert s = sum $ map (log . (english !!) . (subtract 65) . ord) $ filter isAlpha s

transform :: String -> Int -> String
transform str n = map (chr . (+65) . (flip mod 26) . (+n) . (subtract 65) . ord) str

caesarCrackTable :: String -> IO ()
caesarCrackTable input = mapM_ putStrLn $ map (toStr) $ caesarCrack input
  where toStr (num, prob) = (show num) ++ " (" ++ (show prob) ++ ")"

isPrime :: Int -> Bool
isPrime x = (x /= 1) && (null $ filter doesDivide (takeWhile (<(ceiling (sqrt (fromIntegral x)))) [2..]))
  where doesDivide n = x `mod` n == 0

getPrimeFactors :: Int -> [Int]
getPrimeFactors 1 = []
getPrimeFactors x = first:getPrimeFactors rest
  where first = head $ filter (\x -> isFactor x && isPrime x) [1..]
        rest = x `div` first
        isFactor n = x `mod` n == 0

every :: Int -> [a] -> [a]
every n [] = []
every n (x:xs) = x : every n (drop (n-1) xs)

vigniere :: Int -> String -> [[(Int, Float)]]
vigniere cols str = map caesarCrack . map strIntoCols $ [0..(cols-1)]
  where strIntoCols x = every cols (drop x str)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [1..]

vigenereTable :: Int -> String -> IO ()
vigenereTable cols str = do
  forM_ (zipWithIndex computed) $ \(index, info) -> do
    putStrLn $ "Column " ++ (show index) ++ ":"
    putStrLn $ ""
    mapM_ putStrLn $ map toStr info
    putStrLn $ ""
  where computed = vigniere cols str 
        toStr (num, prob) = (show num) ++ " (" ++ (show prob) ++ ")"
