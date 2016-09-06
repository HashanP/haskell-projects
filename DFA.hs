import Data.List (union)

class Automaton m where
  evaluate ::  m s -> [s] -> Bool

-- Based on: https://cseweb.ucsd.edu/classes/wi14/cse105-a/haskell/intro.html<Paste>
data DFA q s = DFA 
  {getStates :: [q]
  ,getInputs :: [s]
  ,getTransition :: (q -> s -> q)
  ,getInitial :: q
  ,getFinal :: [q]
  }
-- where q refers to some finite set of states
-- and s the input alphabet (a finite set of symbols)

instance (Eq q) => Automaton (DFA q) where
  evaluate dfa str = foldl f (getInitial dfa) str `elem` getFinal dfa
    where f = getTransition dfa

-- Test with a simple automaton to accept the language 0*1*
states1 = [0, 1, 2]
dead1 = 2

inputs1 = [0, 1]
final1 = [0, 1]
initial1 = 0
transition1 :: Int -> Int -> Int
transition1 0 0 = 0
transition1 0 1 = 1
transition1 1 1 = 1
transition1 _ _ = dead1

dfa1 = DFA states1 inputs1 transition1 initial1 final1

-- Another automaton to accept the language 00*1
states2 = [0, 1, 2, 3, 4]
dead2 = 2

inputs2 = [0, 1]
final2 = [1]
initial2 = 0
transition2 :: Int -> Int -> Int
transition2 0 0 = 3
transition2 3 0 = 3
transition2 3 1 = 1
transition2 _ _ = dead2

dfa2 = DFA states2 inputs2 transition2 initial2 final2

createProductAutomaton :: DFA q1 s -> DFA q2 s -> [(q1,q2)] -> DFA (q1,q2) s
createProductAutomaton a b finals = DFA allStates (getInputs a) transitionF (getInitial a, getInitial b) finals
  where allStates = [(x,y) | x <- (getStates a), y <- (getStates b)]
        transitionF (q1,q2) s = (getTransition a q1 s, getTransition b q2 s)

unionDFA a b = createProductAutomaton a b allFinals
  where allFinals = union [(x,y) | x <- (getFinal a), y <- (getStates b)] [(x,y) | x <- (getStates a), y <- (getFinal b)]

intersectDFA a b = createProductAutomaton a b allFinals
  where allFinals = [(x,y) | x <- (getFinal a), y <- (getFinal b)] 

main = do
--  print $ evaluate dfa1 [0,1,1] 
  print $ evaluate (intersectDFA dfa1 dfa2) [1] 
