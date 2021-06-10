{-# LANGUAGE GADTs                                                         #-}
{-# LANGUAGE TypeFamilies                                                  #-}
{-# LANGUAGE TypeOperators                                                 #-}

-- Simply-typed lambda calculus

module STLC where

import Data.Type.Equality

-- Let's encode STLC typing invariants in to the expression data type
--
-- We start by equating type-level representations of type (i.e. Int, Bool, Int -> Bool) with term level representation of types
-- This allows us to embed type information into expressions
data Type :: * -> * where
  TInt   :: Type Int
  TBool  :: Type Bool
  TArrow :: Type a -> Type b -> Type (a -> b)

-- Boolean operators :D
data Bop :: * -> * -> * where
  Add :: Bop Int Int
  Sub :: Bop Int Int
  Eq  :: Bop Int Bool
  Lt  :: Bop Int Bool
  Gt  :: Bop Int Bool
  And :: Bop Bool Bool
  Or  :: Bop Bool Bool

-- A class that obtains the term-level type of an STLC expression
class TypeOf a where
  typeOf :: a -> Type a

instance TypeOf Int where
  typeOf _ = TInt

instance TypeOf Bool where
  typeOf _ = TBool

-- Expression data type
data Expr :: * -> * where
  Lit    :: TypeOf a => a -> Expr a
  Var    :: String -> Type a -> Expr a
  Lambda :: String -> Type a -> Expr b -> Expr (a -> b)
  App    :: Expr (a -> b) -> Expr a -> Expr b
  Bop    :: Bop a b -> Expr a -> Expr a -> Expr b
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a
  Lift   :: a -> Type a -> Expr a

-- Some simple programs
--plus :: Expr (Int -> Int -> Int)
--plus = Lambda "x" TInt $ Lambda "y" TInt $ Bop Add (Var "x") (Var "y")
--
--abs :: Expr (Int -> Int)
--abs = Lambda "x" TInt $ If (Bop Lt (Var "x") (Lit 0))
--                           (Bop Sub (Lit 0) (Var "x"))
--                           (Var "x")
--


-- Now, let's work towards writing an interpreter

-- First, we need to get static type information of *expressions*
class TypeOfExpr a where
  typeOfExpr :: Expr a -> Type a

instance TypeOfExpr Int where
  typeOfExpr _ = TInt

instance TypeOfExpr Bool where
  typeOfExpr _ = TBool

-- now, we also need TypeOfExpr of arrow type
-- the instance relies on the structure of the expression
instance TypeOfExpr b => TypeOfExpr (a -> b) where
    typeOfExpr (Var _ t)      = t
    typeOfExpr (Lambda _ t e) = TArrow t $ typeOfExpr e
    typeOfExpr (App e1 _)     = case typeOfExpr e1 of
                                  TArrow _ t2 -> t2
    typeOfExpr (If _ e1 _)    = typeOfExpr e1 -- which is the same at typeOfExpr e2
    typeOfExpr (Lift _ t)     = t

-- Then, we need a way to prove type equality
-- the Data.Type.Equality module defines (:~:), which can be thought of as a theorem stating the equality
-- between types. The only proof of this theorem is the data constructor Refl: any type is equal to itself
eqTy :: Type u -> Type v -> Maybe (u :~: v)
eqTy TInt TInt   = Just Refl
eqTy TBool TBool = Just Refl
eqTy (TArrow u1 u2) (TArrow v1 v2) = do
  Refl <- eqTy u1 v1
  Refl <- eqTy u2 v2
  return Refl
eqTy _   _       = Nothing
-- i.e., Int == Int, Bool == Bool, and two arrow types are equal if their input and output types are equal

-- Now, we need to find a way to do *substitution*--i,e, when a lambda abstraction is applied to an argument, all occurences of the input variable in the function body get substituted for the argument
subst :: String -> u -> Type u -> Expr t -> Expr t
subst _ _ _ (Lit b) = Lit b
subst x v u (Var y t)
  | x == y   = case eqTy u t of
                  Just Refl -> Lift v u
                  Nothing   -> error "ill-typed substitution" -- this is a design choice. We could also have allowed variables of different types to have the same identifier (but this is arguably not very idiomatic)
 | otherwise = Var y t
subst x v u (Bop b e1 e2) = Bop b
                            (subst x v u e1)
                            (subst x v u e2)
subst x v u (If e1 e2 e3) = If (subst x v u e1)
                               (subst x v u e2)
                               (subst x v u e3)
subst x v u (Lambda y t e) 
  | x == y    = Lambda y t e
  | otherwise = Lambda y t (subst x v u e)
subst x v u (App e1 e2)   = App (subst x v u e1) (subst x v u e2)
subst _ _ _ (Lift x t)      = Lift x t

-- and we're finally ready to write the interpreter!
eval :: Expr t -> t
eval (Lit v)        = v
eval (Var _ _)      = error "Free variable has no value"
eval (Lambda x t e) = \v -> eval $ subst x v t e
eval (App e1 e2)    = (eval e1) (eval e2)
eval (Bop b e1 e2)  = (evalBop b) (eval e1) (eval e2)
eval (If b e1 e2)   | eval b    = eval e1
                    | otherwise = eval e2
eval (Lift x _)     = x

evalBop :: Bop a b -> a -> a -> b
evalBop Add = (+)
evalBop Sub = (-)
evalBop Eq  = (==)
evalBop Lt  = (<)
evalBop Gt  = (>)
evalBop And = (&&)
evalBop Or  = (||)



