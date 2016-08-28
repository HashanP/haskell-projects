true x y = x
false x y = y

-- not and negate are taken by the Prelude
inv x = x false true
-- Logical conjunction (AND), and is also taken by the Prelude
conj a b = a (b true false) false
-- Logical disjunction (OR), or is also taken by the Prelude
disj a b = a true (b true false)
-- XOR isn't taken
xor a b = a (b false true) (b true false)
-- But all we really needed, to implement was NAND
nand a b = a false (b false true)

toBool x = x True False
