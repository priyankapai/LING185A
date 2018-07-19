module Assignment02 where

data Numb = Z | S Numb deriving Show

add :: Numb -> Numb -> Numb
add m n = case n of {Z -> m; S n' -> S (add m n')}

one, two, three, four, five, six :: Numb
one = S Z
two = S one
three = S two
four = S three
five = S four
six = S five

data NumbList = EmptyNL | NonEmptyNL Numb NumbList deriving Show

list0, list1, list2 :: NumbList
list0 = NonEmptyNL one (NonEmptyNL two (NonEmptyNL three EmptyNL))
list1 = NonEmptyNL four (NonEmptyNL Z (NonEmptyNL two EmptyNL))
list2 = NonEmptyNL six (NonEmptyNL one (NonEmptyNL three (NonEmptyNL four EmptyNL)))

-----------------------------------------------------------------
-----------------------------------------------------------------
-- IMPORTANT: Do not change anything above here.
--            Write all your code below.
-----------------------------------------------------------------
-----------------------------------------------------------------

------------------------------------
-- Recursive functions on Numb type
------------------------------------

--sumUpTo
sumUpTo :: Numb -> Numb
sumUpTo x =
  case x of
  Z -> Z
  S x' -> add x (sumUpTo x')

--equal
equal :: Numb -> (Numb -> Bool)
equal n m = 
  case n of
  Z -> 
    case m of 
    Z-> True
    S m' -> False
  S n' -> 
    case m of 
    Z-> False
    S m' -> equal n' m' 

--difference
difference :: Numb -> (Numb -> Numb)
difference n m = 
  case n of
  Z -> m
  S n' -> 
    case m of 
    Z -> n
    S m' -> difference n' m'


----------------------------------------
-- Recursive functions on lists of Numbs
----------------------------------------

--total
total :: NumbList -> Numb
total nl =
  case nl of
  EmptyNL -> Z
  NonEmptyNL n nl'-> add n (total nl')

--incrementAll
incrementAll :: Numb -> (NumbList -> NumbList)
incrementAll x nl =
  case nl of
  EmptyNL -> EmptyNL
  NonEmptyNL n nl' -> NonEmptyNL (add n x) (incrementAll x nl')

--addToEnd
addToEnd x nl =
  case nl of
  EmptyNL -> NonEmptyNL x nl
  NonEmptyNL n nl' -> NonEmptyNL  n (addToEnd x nl')

--lastElement
lastElement :: NumbList -> Numb
lastElement nl =
  case nl of
  EmptyNL -> Z
  NonEmptyNL n nl' -> 
    case nl' of
    EmptyNL -> n
    NonEmptyNL n'' nl'' -> lastElement nl'

--contains
contains :: (Numb -> Bool) -> (NumbList -> Bool) 
contains f nl =
  case nl of 
  EmptyNL -> False
  NonEmptyNL n nl' -> 
    case (f n) of
    True -> True
    False -> contains f nl'

--remove
remove :: (Numb -> Bool) -> (NumbList -> NumbList)
remove f nl = 
  case (contains f nl) of
  False -> nl
  True -> 
    case nl of
    EmptyNL -> nl
    NonEmptyNL n nl' -> 
      case (f n) of
      False -> NonEmptyNL n (remove f nl')
      True -> remove f nl'

--append
append :: NumbList -> (NumbList -> NumbList)
append nl1 nl2 =
  case nl1 of
  EmptyNL -> nl2
  NonEmptyNL n nl1' ->
    case nl1' of
    EmptyNL -> NonEmptyNL n (append EmptyNL nl2)
    NonEmptyNL n' nl1'' -> NonEmptyNL n (append nl1' nl2)

--prefix
prefix :: Numb -> (NumbList -> NumbList)
prefix n l =
  case n of
  Z -> EmptyNL
  S n' -> 
    case l of
    EmptyNL -> EmptyNL
    NonEmptyNL x l' -> NonEmptyNL x (prefix n' l')

     
