data Symbol = Blank | Zero | One
  deriving (Eq, Ord, Show)
data Direction = L | R
  deriving (Eq, Ord, Show)
type State = Int

data TuringMachine = TuringMachine {
  getTransition :: (State -> Symbol -> (State, Symbol, Direction))
, getLeft :: [Symbol]
, getRight :: [Symbol]
, getFinal :: [Int]
, getState :: Int -- -1 considered as halting
} 

instance Show TuringMachine where
  show t = (show $ getLeft t) ++ " " ++ (show $ getState t) ++ " " ++ (show $ getRight t)

-- Simulate 1 Move of the Turing Machine
move :: TuringMachine -> TuringMachine
move t
  | direction == L = TuringMachine transition (init left') ((last left'):newSymbol:(tail right)) final newState
  | direction == R = TuringMachine transition (left ++ [newSymbol]) (tail right) final newState
  where state = getState t
        symbol = head right
        (newState, newSymbol, direction) = transition state symbol
        transition = getTransition t
        left = getLeft t
        left' 
          | null left = [Blank]
          | otherwise = left
        right
          | null (getRight t) = [Blank]
          | otherwise = getRight t
        final = getFinal t

move' :: (Integral a) => TuringMachine -> a -> TuringMachine
move' t 0 = t
move' t x = move' (move t) (x - 1)

state1 = 0
left1 = [One,Zero]
right1 = [Zero]
final1 = [1]
transition1 _ Blank = (-1, Blank, L)
transition1 _ Zero = (0, Blank, L)
transition1 _ One = (1, One, L)

evaluate :: TuringMachine -> Bool
evaluate t
  | getState t `elem` getFinal t = True
  | getState t == -1 = False
  | otherwise = evaluate (move t)

main = do
  print $ evaluate (TuringMachine transition1 left1 right1 final1 state1)
