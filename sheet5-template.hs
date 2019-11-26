import Data.List (intercalate)

-- Define a data type for our expressions
data Expr = Const Bool
  | Var Char
  | Not Expr
  | And [Expr]
  | Or [Expr]
  deriving Eq

-- Provide some custom pretty-printing
instance Show Expr where
  show (Const b) = show b
  show (Var a) = [a]
  show (Not e) = "¬" ++ show e ++ ""
  show (And es) = "(" ++ intercalate " ∧ " [show e | e <- es] ++ ")"
  show (Or es)  = "(" ++ intercalate " ∨ " [show e | e <- es] ++ ")"

-- Introduce Bindings as a synonym for list of (Char, Bool)
type Bindings = [(Char, Bool)]

-- Remove duplicates from a list
uniquify :: Eq a => [a] -> [a]
uniquify [] = []
uniquify (x:xs) = x : filter (/= x) (uniquify xs)

-- Look up a variable's value in some bindings by name
find :: Char -> Bindings -> Bool
find = undefined

-- Evaluate an expression with variables bound to values to produce a boolean
eval :: Expr -> Bindings -> Bool
eval = undefined

-- Find the names of all free variables in an expression
vars :: Expr -> [Char]
vars = undefined

-- Enumerate a list of all boolean assignments to lists of length n
bools :: Int -> [[Bool]]
bools = undefined

-- Given an expression, produce a list of bindings
-- where each set of bindings is one of the possible assignments of
-- boolean values to the free variables in the expression
bindings :: Expr -> [Bindings]
bindings = undefined

-- Try and SAT an expression given a list of bindings
sat' :: Expr -> [Bindings] -> (Bool, Bindings)
sat' = undefined

-- Is a boolean expression SAT? If yes, return (True, bindings that
-- satisfy)
-- otherwise, return (False, [])
sat :: Expr -> (Bool, Bindings)
sat = undefined
