-- Based on: https://cseweb.ucsd.edu/classes/wi14/cse105-a/haskell/intro.html<Paste>
data DFA q s = DFA 
  {states :: [q]
  ,inputs :: [s]
  ,transition :: (q -> s -> q)
  ,initial :: q
  ,final :: [q]
  }
-- where q refers to some finite set of states
-- and s the input alphabet (a finite set of symbols)

evaluate :: (Eq q) => DFA q s -> [s] -> Bool
evaluate dfa str = foldl f (initial dfa) str `elem` final dfa
  where f  = transition dfa


-- Test with a simple automaton to accept the language 0*1*
states' = [0, 1, 2]
dead = 2

inputs' = [0, 1]
final' = [0, 1]
initial' = 0
transition' :: Int -> Int -> Int
transition' 0 0 = 0
transition' 0 1 = 1
transition' 1 0 = dead
transition' 1 1 = 1

dfa' = DFA states' inputs' transition' initial' final'

main = do
  print $ evaluate dfa' [0,1,0] 
