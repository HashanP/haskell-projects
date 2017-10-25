-- A way of representing arbitary propositional formulae 
data AFormula = AConj AFormula AFormula 
              | ADisj AFormula AFormula
              | AImpl AFormula AFormula
              | ALit Int
              | ANeg AFormula

-- A way of rpresenting implication-free propositional formulae
data BFormula = BConj BFormula BFormula
              | BDisj BFormula BFormula
              | BLit Int
              | BNeg BFormula

-- A way of representing formulae in negation normal form
data CLit' = CPos Int | CNeg Int

data CFormula = CConj CFormula CFormula
              | CDisj CFormula CFormula
              | CLit CLit'

-- A way of represneting formula in conjunctive normal form
data Literal = Pos Int | Neg Int deriving (Show)
type Disjunction = [Literal]
type Conjunction = [Disjunction]

implFree :: AFormula -> BFormula
implFree (AConj a b) = BConj (implFree a) (implFree b)
implFree (ADisj a b) = BDisj (implFree a) (implFree b)
implFree (AImpl a b) = BDisj (BNeg (implFree a)) (implFree b)
implFree (ALit a) = BLit a
implFree (ANeg a) = BNeg (implFree a)

nnf :: BFormula -> CFormula
nnf (BConj a b) = CConj (nnf a) (nnf b)
nnf (BDisj a b) = CDisj (nnf a) (nnf b)
nnf (BLit a) = CLit (CPos a)
nnf (BNeg (BLit a)) = CLit (CNeg a)
nnf (BNeg (BConj a b)) = CDisj (nnf (BNeg a)) (nnf (BNeg b))
nnf (BNeg (BDisj a b)) = CConj (nnf (BNeg a)) (nnf (BNeg b))
nnf (BNeg (BNeg a)) = nnf a

distr :: Conjunction -> Conjunction -> Conjunction
distr (a1:[]) (a2:[]) = [a1 ++ a2]
distr (a:as) b = distr [a] b ++ distr as b
distr a (b:bs) = distr a [b] ++ distr a bs

cnf :: CFormula -> Conjunction
cnf (CLit (CPos a)) = [[Pos a]]
cnf (CLit (CNeg a)) = [[Neg a]]
cnf (CConj a b) = cnf a ++ cnf b
cnf (CDisj a b) = distr (cnf a) (cnf b)
