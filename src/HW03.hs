module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend oldState str val = state
    where state k
            |k == str = val
            |otherwise = oldState k

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

bopTransform op a b = case op of
                Plus -> (+) a b
                Minus -> (-) a b
                Times -> (*) a b
                Divide -> div a b
                Gt -> boolToInt((>) a b)
                Ge -> boolToInt((>=) a b)
                Lt -> boolToInt((<) a b)
                Le -> boolToInt((<=) a b)
                Eql -> boolToInt((==) a b)

evalE :: State -> Expression -> Int
evalE st exp = case exp of
            Var str -> st str
            Val n -> n
            Op exp' bop exp'' -> bopTransform bop (evalE st exp') (evalE st exp'')

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar st = case st of
            Assign a exp -> DAssign a exp
            Incr a -> DAssign a (Op (Var a) Plus (Val 1))
            If exp st1 st2 -> DIf exp (desugar st1) (desugar st2)
            While exp st1 -> DWhile exp (desugar st1)
            For st1 exp st2 st3 -> DSequence (desugar st1) (DWhile exp (DSequence (desugar st3) (desugar st2)))
            Sequence st1 st2 -> DSequence (desugar st1) (desugar st2)
            _ -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st ds = case ds of
        DAssign str exp -> extend st str (evalE st exp)
        DIf exp ds1 ds2 -> if ((evalE st exp) == 1) then (evalSimple st ds1) else (evalSimple st ds2)
        DWhile exp ds1  -> if ((evalE st exp) == 1) then (evalSimple st (DSequence ds1 (DWhile exp ds1))) else st
        DSequence ds1 ds2 -> evalSimple (evalSimple st ds1) ds2
        DSkip -> st

run :: State -> Statement -> State
run st stm = evalSimple st (desugar stm)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]

