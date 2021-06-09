{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week8.HW8 where

import Prelude (Show(..), Eq(..), ($), (.), flip)

-- Propositional Logic --------------------------------
-- This part was given to us

-- False, the uninhabited type
data False

-- Logical Not
type Not p = p -> False

-- Logical Disjunction
data p \/ q = Left  p
            | Right q

-- Logical Conjunction
data p /\ q = Conj p q

-- If and only if
type p <-> q = (p -> q) /\ (q -> p)

-- Admit is used to assume an axiom without proof
admit :: p
admit = admit

-- There is no way to prove this axiom in constructive logic, therefore we
-- leave it admitted
excludedMiddle :: p \/ Not p
excludedMiddle = admit

absurd :: False -> p
absurd false = admit

doubleNegation :: p <-> Not (Not p)
doubleNegation = Conj (\p not_p -> not_p p) admit

modusPonens :: (p -> q) -> p -> q
modusPonens = ($)

modusTollens :: (p -> q) -> Not q -> Not p
modusTollens = flip (.)

materialImplication :: (p -> q) <-> (Not p \/ q)
-- The proof has two parts, the forward direction (->) and
--   the backwards direction (<-)
materialImplication = Conj dir1 dir2
    where 
      -- Case 1: (P -> Q) -> (~P \/ Q)
      dir1 p_imp_q =
          -- There are 2 cases, P and ~P
          case excludedMiddle of
            -- SCase 1: P, then Q since P -> Q
            Left  p     -> Right $ p_imp_q p
            -- SCase 2: ~P, then ~P
            Right not_p -> Left not_p
      -- Case 2: (~P \/ Q) -> (P -> Q)
      -- SCase 1: ~P -> (P -> Q)
      dir2 (Left not_p) p =
          -- This is a contradiction since we have both
          -- P and ~P
          absurd $ not_p p
      -- SCase 2: Q -> (P -> Q)
      dir2 (Right q)    _ =
          -- q is a witness for the proposition Q,
          -- therefore we can just return it
          q

-- Exercise 1 -----------------------------------------
-- Prove the disjunctive syllogism
disjunctiveSyllogism :: (p \/ q) -> Not p -> q
disjunctiveSyllogism (Left p) not_p  = absurd $ not_p p
disjunctiveSyllogism (Right q) not_p = q

-- Exercise 2 -----------------------------------------
-- Prove the composition theorem
composition :: (p -> q) \/ (p -> r) -> p -> q \/ r
composition (Left pq) p  =  Left $ pq p
composition (Right pr) p = Right $ pr p 

-- Exercise 3 -----------------------------------------
-- Prove the transposition theorem:
transposition :: (p -> q) <-> (Not q -> Not p)
transposition = Conj dir1 dir2
  where 
    dir1        = modusTollens
    dir2 nqnp p = unDoubleNegate $ modusTollens nqnp (doubleNegate p) 

-- Helper functions
-- Firstly, we write helper functions to select a part of a conjunction
-- I think this is an okay thing to do. Both parts of a conjunction must be true, so the output is true as well 
getFirst :: (p /\ q) -> p
getFirst (Conj p _) = p

getSecond :: (p /\ q) -> q
getSecond (Conj _ q) = q

-- Then, we can use the doubleNegation axiom to write functions that doubly negate a proposition, or undo a double negation
doubleNegate :: p -> Not (Not p)
doubleNegate = getFirst doubleNegation

unDoubleNegate :: Not (Not p) -> p
unDoubleNegate = getSecond doubleNegation

-- Exercise 4 -----------------------------------------
-- Prove one of deMorgan's laws:
deMorgan :: Not (p \/ q) <-> (Not p /\ Not q)
deMorgan = Conj dir1 dir2
  where
    dir1 not_porq          = Conj (not_porq . Left) (not_porq . Right)
    dir2 (Conj np nq) porq = nq $ disjunctiveSyllogism porq np
    
-- Natural Numbers ------------------------------------
-- this part was given to us
data Nat = O | S Nat
           deriving (Show, Eq)

type family (n :: Nat) + (m :: Nat) :: Nat
type instance O     + m = m
type instance (S n) + m = S (n + m)
infixl 6 +

data Forall n where
    Zero :: Forall O
    Succ :: Forall n -> Forall (S n)

data (n :: Nat) == (m :: Nat) where
    Refl :: n == n
infix 4 ==

type (n :: Nat) /= (m :: Nat) = Not (n == m)
infix 4 /=

data n < m where
  LT_Base :: O < S n
  LT_Rec  :: n < m -> S n < S m

type n >  m = m < n
type n <= m = (n < m) \/ (n == m)
type n >= m = m <= n

-- Weakening Lemma
neqWeaken :: S n /= S m -> n /= m
neqWeaken h_neq Refl = h_neq Refl

{- ********************************************************
   * Theorem: Not Equal Implies Greater Than or Less Then *
   ******************************************************** -}
neqGtLt :: Forall n -> Forall m ->
             n /= m -> (n < m) \/ (n > m)
-- The proof is by induction on n and m
-- Base Case 1: both n and m are 0. This is impossible since the hypothesis h
--   states that n /= m
neqGtLt Zero  Zero        h = absurd $ h Refl
-- Base Case 2: n == 0 and m > 0. Here we choose the left case, n < m
neqGtLt Zero (Succ m)     _ = Left  LT_Base
-- Base Case 3: n > 0 and m == 0. Here we choose the right case, n > m
neqGtLt (Succ n) Zero     _ = Right LT_Base
-- Inductive Step: both n and m are greater than 0
neqGtLt (Succ n) (Succ m) h_neq =
    -- We generate an induction hypothesis by invoking a recursive call on n,
    -- m, and the weakening hypothesis
    case neqGtLt n m (neqWeaken h_neq) of
      -- Case 1: n < m with witness w. This means that S n < S m
      Left  w -> Left  $ LT_Rec w
      -- Case 2: n > m with witness w. This means that S n > S m
      Right w -> Right $ LT_Rec w

-- Exercise 5 -----------------------------------------
-- Prove the addZero theorem 

-- (oPlusN and nPlus0 were given to us)
oPlusN :: O + n == n
oPlusN = Refl

nPlus0 :: Forall n -> n + O == n
-- Base Case:
nPlus0  Zero    = Refl {- :: O + O == O -}
-- Inductive Step:
nPlus0 (Succ n) = case nPlus0 n of
                      Refl   {- :: n   + O == n   -} -> 
                        Refl {- :: S n + O == S n -}

-- Prove the addZero theorem
addZero :: Forall n -> O + n == n + O
addZero Zero = Refl
addZero (Succ n) = case addZero n of
                    Refl -> Refl

-- Exercise 6 -----------------------------------------
-- Prove that n < n + 1 for all n
nLtSn :: Forall n -> n < S n
nLtSn Zero = LT_Base
nLtSn (Succ n) = case nLtSn n of
                   w -> LT_Rec w

-- Exercise 7 -----------------------------------------
-- Prove that the successor of an odd number is even

-- the following definitions are given to us:
data Even :: Nat -> * where
    E_Zero :: Even O
    E_Rec  :: Even n -> Even (S (S n))

data Odd :: Nat -> * where
    O_One :: Odd (S O)
    O_Rec :: Odd n -> Odd (S (S n))

evenPlusOne :: Even n -> Odd (S n)
-- Base Case: The successor of zero is odd
evenPlusOne  E_Zero   = O_One
-- Inductive Step: if S (S n) is even then S (S (S n)) is odd
evenPlusOne (E_Rec n) = O_Rec $ evenPlusOne n

-- Proof that the successor of an odd number is even
oddPlusOne :: Odd n -> Even (S n)
oddPlusOne O_One     = E_Rec E_Zero
oddPlusOne (O_Rec n) = E_Rec $ oddPlusOne n

-- Exercise 8 -----------------------------------------
-- Prove that, for any n, n + n is even

-- The following lemma is given to us:
succSum :: Forall n -> Forall m ->
            S n + S m == S (S (n + m))
succSum  Zero    m = Refl
succSum (Succ n) m = case succSum n m of
                        Refl -> Refl

-- For all n, n + n is even
doubleEven :: Forall n -> Even (n + n)
doubleEven Zero     = E_Zero
doubleEven (Succ n) = case succSum n n of
                        Refl -> E_Rec $ doubleEven n



