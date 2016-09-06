import Data.ByteString (ByteString, readFile, sort, group, head, length, unpack)
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (sortBy, foldl1')
import Data.Word (Word8)
import qualified Data.Map.Strict as M

data BinaryTree = Terminal Int Word8 | Node Int BinaryTree BinaryTree
  deriving (Show)

toPretty :: BinaryTree -> [String]
toPretty (Terminal len x) = ["Terminal: " ++ (show len) ++ ", " ++ (show x)]
toPretty (Node len x y) = ("Node: " ++ (show len)):((map ('-':) (toPretty x)) ++ (map ('-':) (toPretty y)))

toTable :: BinaryTree -> [(Word8, String)]
toTable (Terminal len x) = [(x, "")]
toTable (Node len x y) = (append '0' $ toTable x) ++ (append '1' $ toTable y)
  where append x = map (\(a,b) -> (a, x:b))

weight :: BinaryTree -> Int
weight (Terminal x _) = x
weight (Node x _ _) = x

toPair :: ByteString -> (Word8, Int)
toPair str = (Data.ByteString.head str, Data.ByteString.length str)

orderedInsert :: BinaryTree -> [BinaryTree] -> [BinaryTree]
orderedInsert y l@(x:xs)
  | weight x > weight y = y:l
  | otherwise = x: orderedInsert y xs
orderedInsert y [] = [y]

collapse :: [BinaryTree] -> BinaryTree
collapse (x:y:xs) = collapse $ orderedInsert (Node (weight x + weight y) y x) xs
collapse (x:[]) = x

--toStr :: M.Map Word8 String -> ByteString -> [String]
toStr table = concat . map ((table M.!))
  where f (Just x) = x

main :: IO ()
main = do
  contents2 <- Data.ByteString.readFile "./xterm.js"
  let contents = group . sort $ contents2
  let cps = sortBy (compare `on` snd) . map toPair $ contents
  putStrLn $ "Table showing frequency of bytes:"
  mapM_ print cps
  let table = toTable . collapse . map (\(x,y) -> Terminal y x) $ cps
  putStrLn $ "Table converting bytes to binary strings:"
  mapM_ print table
  putStrLn $ "Bits in original file: " ++ show ((Data.ByteString.length contents2) * 8)
  putStrLn $ "Bits after being Huffman encoded: " ++ (show $ Prelude.length $ toStr (M.fromList table) (unpack contents2))
