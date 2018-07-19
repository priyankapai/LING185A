module ContextFree where

------------------------------------------------------------------------------------

data Cat = S | NP | VP | V | D | N | PP | P | IV | TV | RC deriving (Show,Eq)

data StrucDesc  = Binary Cat StrucDesc StrucDesc
                | Unary Cat StrucDesc
                | Leaf Cat String
                deriving Show

data GrammarRule    = BinaryStep Cat Cat Cat
                    | UnaryStep Cat Cat
                    | End Cat String
                    deriving Show

type Address = [Int]

grammar1 =  [   -- S rules
                BinaryStep S NP VP,
                -- VP rules
                BinaryStep VP V NP,
                BinaryStep VP V S,
                BinaryStep VP VP PP,
                UnaryStep  VP V,
                -- PP rules
                BinaryStep PP P NP,
                -- D rules
                End D "the",
                End D "a",
                -- N rules
                End N "cat",
                End N "dog",
                -- NP rules
                BinaryStep NP D N,
                End NP "John",
                End NP "Mary",
                -- V rules
                End V "left",
                End V "thinks",
                -- P rules
                End P "with"
            ]

grammar2 = [    BinaryStep S NP IV,
                BinaryStep NP N RC,
                UnaryStep NP N,
                BinaryStep RC NP TV,
                End N "dogs",   End N "cats",
                End IV "chase", End IV "sleep",
                End TV "chase"
            ]

sd1 = Binary S (Leaf NP "John") (Binary VP (Leaf V "left") (Leaf NP "Mary"))

sd2 = Binary S (Leaf NP "John")
               (Binary VP (Unary VP (Leaf V "left")) 
                          (Binary PP (Leaf P "with") (Binary NP (Leaf D "a") (Leaf N "cat")))
               )

sd3 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd1)

sd4 = Binary S (Binary NP (Leaf D "the") (Leaf N "dog")) (Binary VP (Leaf V "thinks") sd2)

sd5 = Binary S (Binary NP (Leaf N "dogs") (Binary RC (Unary NP (Leaf N "dogs")) (Leaf TV "chase"))) (Leaf IV "chase")

------------------------------------------------------------------------------------

pf :: StrucDesc -> [String]
pf (Binary c sd1 sd2) = pf sd1 ++ pf sd2
pf (Unary c sd) = pf sd
pf (Leaf c s) = [s]

leftmostLeaf :: StrucDesc -> String
leftmostLeaf (Leaf c s) = s
leftmostLeaf (Unary c sd) = leftmostLeaf sd
leftmostLeaf (Binary c sd1 sd2) = leftmostLeaf sd1

categoryOf :: StrucDesc -> Cat
categoryOf (Leaf c s) = c
categoryOf (Unary c sd) = c
categoryOf (Binary c sd1 sd2) = c

wellFormed :: [GrammarRule] -> StrucDesc -> Bool
wellFormed g (Leaf c s) = elem c (enders g s)
wellFormed g (Unary c sd) = elem c (predecessorsUnary g (categoryOf sd)) && wellFormed g sd
wellFormed g (Binary c sd1 sd2) = elem c (predecessorsBinary g (categoryOf sd1, categoryOf sd2)) 
                                            && wellFormed g sd1 && wellFormed g sd2

depth :: StrucDesc -> Int
depth (Leaf c s) = 1
depth (Unary c sd) = 1 + depth sd
depth (Binary c sd1 sd2) = if (depth sd1 > depth sd2) then (1 + depth sd1) else (1 + depth sd2)

enders :: [GrammarRule] -> String -> [Cat]
enders [] x = []
enders (r:rs) x =
    case r of
    End c s -> if s == x then c : (enders rs x) else enders rs x
    UnaryStep c ch -> enders rs x
    BinaryStep c ch1 ch2 -> enders rs x

predecessorsUnary :: [GrammarRule] -> Cat -> [Cat]
predecessorsUnary [] x = []
predecessorsUnary (r:rs) x =
    case r of
    End c s -> predecessorsUnary rs x
    UnaryStep c ch -> if ch == x then (c : (predecessorsUnary rs x)) else (predecessorsUnary rs x)
    BinaryStep c ch1 ch2 -> predecessorsUnary rs x

predecessorsBinary :: [GrammarRule] -> (Cat,Cat) -> [Cat]
predecessorsBinary [] x = []
predecessorsBinary (r:rs) x =
    case r of
    End c s -> predecessorsBinary rs x
    UnaryStep c ch -> predecessorsBinary rs x
    BinaryStep c ch1 ch2 -> if (ch1,ch2) == x then (c : (predecessorsBinary rs x)) else (predecessorsBinary rs x)

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

brackets :: StrucDesc -> String
brackets (Binary c sd1 sd2) = "[" ++ brackets sd1 ++ brackets sd2 ++ "]"
brackets (Unary c sd) = "[" ++ brackets sd ++ "]"
brackets (Leaf c s) = s ++ " "

labeledBrackets :: StrucDesc -> String
labeledBrackets (Binary c sd1 sd2) = "[" ++ (show c) ++ " " ++  labeledBrackets sd1 ++ labeledBrackets sd2 ++ "]"
labeledBrackets (Unary c sd) = "[" ++ (show c) ++ " " ++  labeledBrackets sd ++ "]"
labeledBrackets (Leaf c s) = s ++ " " 

numNPs :: StrucDesc -> Int
numNPs (Binary c sd1 sd2) = if (show c) == "NP" then 1+(numNPs sd1 + numNPs sd2) else numNPs sd1 + numNPs sd2
numNPs (Unary c sd) = if (show c) == "NP" then 1+numNPs sd else numNPs sd
numNPs (Leaf c s) = if (show c) == "NP" then 1 else 0

numViolations :: [GrammarRule] -> StrucDesc -> Int
numViolations g (Leaf c s) = if findRule g (Leaf c s) then 0 else 1
numViolations g (Binary c sd1 sd2) = if findRule g (Binary c sd1 sd2) then numViolations g sd1 + numViolations g sd2 
                                     else 1 + (numViolations g sd1) + (numViolations g sd2)
numViolations g (Unary c sd) = if findRule g (Unary c sd) then numViolations g sd else 1 + (numViolations g sd)

findRule :: [GrammarRule] -> StrucDesc -> Bool
findRule [] sd = False
findRule (r:rs) (Leaf c s) = 
  case r of
  UnaryStep c' c'' -> findRule rs (Leaf c s)
  BinaryStep c' c'' c''' -> findRule rs (Leaf c s)
  End c' s' -> if c==c' && s==s' then True else findRule rs (Leaf c s)
findRule (r:rs) (Unary c sd) = 
  case r of
  UnaryStep c' c'' -> if c==c' && c''==(categoryOf sd) then True else findRule rs (Unary c sd)
  BinaryStep c' c'' c''' -> findRule rs (Unary c sd)
  End c' s -> findRule rs (Unary c sd)
findRule (r:rs) (Binary c sd1 sd2)=
  case r of
  UnaryStep c' c'' -> findRule rs (Binary c sd1 sd2)
  BinaryStep c' c'' c''' -> if c==c' && c''==(categoryOf sd1) && c'''==(categoryOf sd2) then True else findRule rs (Binary c sd1 sd2)
  End c' s -> findRule rs (Binary c sd1 sd2)

sdMap :: (String -> String) -> StrucDesc -> StrucDesc
sdMap f (Binary c sd1 sd2) = Binary c (sdMap f sd1) (sdMap f sd2)
sdMap f (Unary c sd) = Unary c (sdMap f sd)
sdMap f (Leaf c s) = Leaf c (f s)

longestPath :: StrucDesc -> [Cat]
longestPath (Leaf c sd1) = [c]
longestPath (Unary c sd) = [c] ++ longestPath sd
longestPath (Binary c sd1 sd2) = 
  if (length (longestPath sd2)) > (length (longestPath sd1)) then ([c] ++longestPath sd2) else ([c]++longestPath sd1)

allPaths :: StrucDesc -> [[Cat]]
allPaths (Leaf c s) = [[c]]
allPaths (Unary c sd) = map (\p -> c : p) (allPaths sd)
allPaths (Binary c sd1 sd2) = 
    map (\p -> c : p) (allPaths sd1) ++ map (\p -> c : p) (allPaths sd2)

addressesOfNPs :: StrucDesc -> [Address]
addressesOfNPs (Unary c sd) = 
    let rest = map (\a -> [0] ++ a) (addressesOfNPs sd) in
    if c == NP then [[]] ++ rest else rest
addressesOfNPs (Binary c sd1 sd2) = 
    let rest = map (\a -> [0] ++ a) (addressesOfNPs sd1) ++ map (\a -> [1] ++ a) (addressesOfNPs sd2) in
    if c == NP then [[]] ++ rest else rest
addressesOfNPs (Leaf c s) = if c == NP then [[]] else []

ccommand :: Address -> Address -> Bool
ccommand (a:as) (r:rs) = if a == r then ccommand as rs else 
  if as == [] then True else False
ccommand [] add2 = False
ccommand add1 [] = False

replace :: StrucDesc -> Address -> StrucDesc -> StrucDesc
replace sd [] r = r
replace (Leaf c s) (n:ns) r = (Leaf c s)
replace (Unary c sd) (n:ns) r = 
    if n == 0 then Unary c (replace sd ns r) else (Unary c sd)
replace (Binary c sd1 sd2) (n:ns) r = 
    case n of 
        0 -> Binary c (replace sd1 ns r) sd2
        1 -> Binary c sd1 (replace sd2 ns r)
        _ -> Binary c sd1 sd2

move :: Address -> StrucDesc -> StrucDesc
move (n:ns) (Binary c sd1 sd2) = Binary c (moveHelp (n:ns) (Binary c sd1 sd2)) (moveTrace (n:ns) (Binary c sd1 sd2))
move (n:ns) (Unary c sd) = Unary c (moveHelp (n:ns) (Binary c sd1 sd2))
move (n:ns) (Leaf c s) = Leaf c s

moveHelp :: Address -> StrucDesc -> StrucDesc
moveHelp (n:ns) (Binary c sd1 sd2) =
   case n of
        0 -> (moveHelp ns sd1) 
        1 ->  (moveHelp ns sd2)
        _ -> Binary c sd1 sd2
moveHelp (n:ns) (Unary c sd) =
   if n==0 then (moveHelp ns sd) else Unary c sd
moveHelp (n:ns) (Leaf c s) =
   (Leaf c s)
moveHelp [] sd = sd

moveTrace :: Address -> StrucDesc -> StrucDesc
moveTrace (n:ns) (Binary c sd1 sd2) =
  let res = moveTrace (n:ns) (Binary c sd1 sd2) in
  case n of
        0 -> Binary c (moveTrace ns sd1) sd2
        1 -> (Binary c sd1 (moveTrace ns sd2))
        _ -> Leaf c "t"
moveTrace (n:ns) (Unary c sd) =
   if n==0 then Unary c (moveTrace ns sd) else Leaf c "t"
moveTrace (n:ns) (Leaf c s) =
   (Leaf c s)
moveTrace [] sd = Leaf (categoryOf sd) "t"
