module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.
import Data.List (foldl')

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty = const 0

-- возвращает состояние, в котором переменная var имеет значение newVal, 
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal = \v -> if v == var then newVal else state v

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state (Var v) = state v
eval _ (Val n) = n
eval state (Op e1 op e2) = opFunc (eval state e1) (eval state e2)
  where
    opFunc = case op of
      Plus -> (+)
      Minus -> (-)
      Times -> (*)
      Divide -> div
      Gt ->  (\x y -> fromEnum (x >  y))
      Ge ->  (\x y -> fromEnum (x >= y))
      Lt ->  (\x y -> fromEnum (x <  y))
      Le ->  (\x y -> fromEnum (x <= y))
      Eql -> (\x y -> fromEnum (x == y))


-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через 
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar (Assign var expr) = DAssign var expr
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (If cond th el) = DIf cond (desugar th) (desugar el)
desugar (While cond body) = DWhile cond (desugar body)
desugar (For init cond update body) = DSequence (desugar init) (DWhile cond (DSequence (desugar body) (desugar update)))
desugar (Block stmts) = foldr DSequence DSkip $ map desugar stmts
desugar Skip = DSkip

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler state (DAssign var expr) = extend state var $ eval state expr
runSimpler state (DIf cond th el) = if eval state cond /= 0 then runSimpler state th else runSimpler state el
runSimpler state loop@(DWhile cond body) = if eval state cond /= 0 then runSimpler newState loop else newState
    where
        newState = runSimpler state body
runSimpler state (DSequence st1 st2) = runSimpler (runSimpler state st1) st2
runSimpler state DSkip = state

{- 
-- 
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state stmt = runSimpler state $ desugar stmt

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot =
  Block
    [ Assign "B" (Val 0)
    , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B"))) (Incr "B")
    , Assign "B" (Op (Var "B") Minus (Val 1))
    ]
{- Вычисление числа Фибоначчи

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
fibonacci =
  If
    (Op (Var "In") Lt (Val 2))
    (Assign "Out" (Val 1))
    (Block
       [ Assign "F0" (Val 1)
       , Assign "F1" (Val 1)
       , For
           (Assign "C" (Val 2))
           (Op (Var "C") Le (Var "In"))
           (Assign "C" (Op (Var "C") Plus (Val 1)))
           (Block
              [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
              , Assign "F0" (Var "F1")
              , Assign "F1" (Var "T")
              , Assign "Out" (Var "T")
              ])
       ]) 
-}
