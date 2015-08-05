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
extend st a b c
  |a == c = b
  |otherwise = st c

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var variable) = st variable
evalE st (Val variable) = variable
evalE st (Op expression bop expression2)
  |bop == Plus = (evalE st expression) + (evalE st expression2)
  |bop == Minus = (evalE st expression) - (evalE st expression2)     
  |bop == Times = (evalE st expression) * (evalE st expression2)   
  |bop == Divide = (evalE st expression) `div` (evalE st expression2)  
  |bop == Gt && (evalE st expression) > (evalE st expression2) = 1
  |bop == Ge && (evalE st expression) >= (evalE st expression2) = 1      
  |bop == Lt && (evalE st expression) < (evalE st expression2) = 1
  |bop == Le && (evalE st expression) <= (evalE st expression2) = 1
  |bop == Eql && (evalE st expression) == (evalE st expression2) = 1
  |otherwise = 0  
  
--let exp = (Op (Val 5) Plus (Var "x"))
--evalE (extend empty "x" 10) exp 

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign string expression) = DAssign string expression
desugar (Incr string) = DAssign string (Op (Val 1) Plus (Var string))  
desugar (If expression statement1 statement2)= DIf expression (desugar statement1) (desugar statement2)
desugar (While expression statement) = DWhile expression (desugar statement)
desugar (Sequence statement1 statement2) = DSequence (desugar statement1) (desugar statement2)
desugar (For statement1 expression statement2 statement3) = 
	DSequence (desugar statement1) (DWhile expression (DSequence (desugar statement2) (desugar statement3)))
desugar (Skip)= DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign string expression) = extend state string (evalE state expression)
evalSimple state (DIf expression dietstatement1 dietstatement2) = 
	if(evalE state expression) /=0 then (evalSimple state dietstatement1) else (evalSimple state dietstatement2)
evalSimple state (DWhile expression dietstatement) = 
	if (evalE state expression) /=0 then evalSimple (evalSimple state dietstatement) (DWhile expression dietstatement) else state
evalSimple state (DSequence dietstatement1 dietstatement2) = evalSimple (evalSimple state dietstatement1) dietstatement2
evalSimple state DSkip = state 

run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

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
