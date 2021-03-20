import Control.Monad (guard)
import Data.List ((\\))

getSolutions 0 _ = return []
getSolutions n m = do
  sol <- getSolutions (n - 1) m
  el <- [0..(m - 1)] \\ sol
  guard $ not (or (zipWith (diagonalThreaten el) sol [1..]))
  return $ el:sol

diagonalThreaten val1 val2 diff =
  val1 + diff == val2 || val1 - diff == val2

solutions = getSolutions 8 8
