import Data.List
import System.IO (readFile)

data Identifier = Unnegated String | Negated String
type Disjunction = [Identifier]
newtype Conjunction = Conjunction { getConj :: [Disjunction] } deriving (Eq)

addToConj :: Conjunction -> Identifier -> Conjunction
addToConj (Conjunction xs) iden = Conjunction $ [iden]:xs

neg :: Identifier -> Identifier
neg (Unnegated str) = Negated str
neg (Negated str) = Unnegated str

{-
a :: Identifier
a = Unnegated "a"

b :: Identifier
b = Unnegated "b"

c :: Identifier
c = Unnegated "c"

d :: Identifier
d = Unnegated "d"

expr :: Conjunction
expr = Conjunction [[neg a,b,c],[neg b],[neg b,a],[neg c,d]] 
-}

isUnitClause :: Disjunction -> Bool
isUnitClause = (== 1) . length

getUnitClauses :: [Disjunction] -> [Identifier]
getUnitClauses = concat . filter isUnitClause

normalise :: Identifier -> String
normalise (Unnegated str) = str
normalise (Negated str) = str

instance Eq Identifier where
  Unnegated str == Unnegated str2 = str == str2
  Negated str == Negated str2 = str == str2
  _ == _ = False

instance Show Identifier where
  show (Unnegated str) = str
  show (Negated str) = "¬" ++ str

showDisj :: Disjunction -> String
showDisj disj = "(" ++ (concat . intersperse " ∨ " $ map show disj) ++ ")"

getAllTokens :: Conjunction -> [String]
getAllTokens (Conjunction xs) = map normalise $ concat xs

instance Show Conjunction where
  show conj = "(" ++ (concat . intersperse " ∧ " $ map showDisj $ getConj conj) ++ ")"

unitPropagation :: Conjunction -> Maybe Conjunction
unitPropagation conj
  | [] `elem` transformed = Nothing
  | null $ getUnitClauses transformed = Just (Conjunction transformed)
  | otherwise = unitPropagation (Conjunction transformed)
  where expr = getConj conj
        transformed = (filter (not . hasUnits) . map removeNegUnits $ expr) 
        units = getUnitClauses expr
        nonUnits = filter (not . isUnitClause) expr
        removeNegUnits xs = filter (\x -> not $ neg x `elem` units) xs
        hasUnits xs = any (\x -> x `elem` units) xs

pureLiteralElimination :: Conjunction -> Maybe Conjunction
pureLiteralElimination conj = Just $ Conjunction $ filter (not . containsTokensToEliminate) expr
  where expr = getConj conj
        allTokens = nub $ concat expr
        tokensToEliminate = filter (\x -> not (neg x `elem` allTokens)) allTokens
        containsTokensToEliminate xs = any (\x -> x `elem` tokensToEliminate) xs

dpll :: Conjunction -> Bool
dpll conj = case simplified of (Just (Conjunction [])) -> True
                               (Just c) -> chooseNext c
                               Nothing -> False
  where simplified = unitPropagation conj >>= pureLiteralElimination
        chooseNext c = dpll (addToConj c idenNextToken) || dpll (addToConj c negIdenNextToken)
          where nextToken = head $ getAllTokens c
                idenNextToken = Unnegated nextToken
                negIdenNextToken = Negated nextToken

parse :: String -> Conjunction
parse str = Conjunction $ map lineToDisjunction $ filter (not . canIgnoreLine) (lines str)
  where canIgnoreLine (x:_) = x == 'c' || x == 'p'
        lineToDisjunction line = map (toIdentifier . read) . init $ words line 
        toIdentifier num = if num < 0
                            then Negated (show $ abs num)
                            else Unnegated (show num)

main = do
  test <- readFile "test.cnf"
  print . dpll $ parse test
