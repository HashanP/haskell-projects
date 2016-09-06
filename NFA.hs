import Data.List (union)

class Automaton m where
  evaluate ::  m s -> [s] -> Bool

-- Based on: https://cseweb.ucsd.edu/classes/wi14/cse105-a/haskell/intro.html<Paste>
data NFA q s = NFA 
  {getStates :: [q]
  ,getInputs :: [s]
  ,getTransition :: q -> s -> [q]
  ,getInitial :: q
  ,getFinal :: [q]
  }
-- where q refers to some finite set of states
-- and s the input alphabet (a finite set of symbols)

instance (Eq q) => Automaton (NFA q) where
  evaluate nfa str = any (`elem` getFinal nfa) (foldl f [getInitial nfa] str)
    where f states input = states >>= flip (getTransition nfa) input

-- Let's make a simple NFA to accept the language (00*1*010 | 01*0001)

states1 = [1..11]
inputs1 = [0, 1]
finals1 = [6, 11]
initial1 = 1

transition1 1 0 = [2, 7] -- Initial state

-- Case 1
transition1 2 0 = [2, 4] 
transition1 2 1 = [3] 
transition1 3 0 = [4]
transition1 3 1 = [3]
transition1 4 1 = [5]
transition1 5 0 = [6]

--Case 2
transition1 7 1 = [7]
transition1 7 0 = [8]
transition1 8 0 = [9]
transition1 9 0 = [10]
transition1 10 1 = [11]
transition1 _ _ = [] -- Dead state

nfa1 = NFA states1 inputs1 transition1 initial1 finals1

main = do
--  print $ evaluate dfa1 [0,1,1] 
  print $ evaluate nfa1 [0, 0, 0, 0, 1]
