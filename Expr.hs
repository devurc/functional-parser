------------------------------------------------------------------------
--Contains a data type for representing:
-- - an arithmetic expression
-- - an expression parser
-- - an expression evaluator
-- - a function for converting the representation into a string

-- TASK: Implement the function value. The expression value e dictionary should
-- return the value of e if all the variables occur in dictionary and there is
-- no division by zero. Otherwise an error should be reported using error

-- Main type is renamed to T
-- The value function should evaluate an expression
-- Will need a dictionary to find the values of the variables
-- Will make heavy use of the parser functions - import parser module

-- We rename the type class Parse to T.

--Need to extend the datatype
------------------------------------------------------------------------

module Expr(Expr, T, parse, fromString, value, toString) where

{-
   An expression of type Expr is a representation of an arithmetic expression
   with integer constants and variables. A variable is a string of upper-
   and lower case letters. The following functions are exported

   parse :: Parser Expr
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.

   fromString :: String -> Expr
   fromString expects its argument to contain an expression and returns the
   corresponding Expr.

   toString :: Expr -> String
   toString converts an expression to a string without unneccessary
   parentheses and such that fromString (toString e) = e.

   value :: Expr -> Dictionary.T String Int -> Int
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.
-}
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary


--Expr values can only be of the following values (it is also somewhat recursive):
data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
         deriving Show

--Type examples include Bool, Char, Int etc. Now the above data values are instances
--of the type T
type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)

------------------------------------------------------------------------
  ----VALUE FUNCTION----
--Implement the function value. The expression value e dictionary should return
--the value of e if all the variables occur in dictionary and there is no division
--by 0.

--The dictionary module is imported. It has the qualified keyword to cause the
--imported names to be prefixed by the name of the module imported.

--To look up something in a dictionary, the syntax is lookup a (Dictionary dict)
--lookup :: (Eq a, Ord a) => a -> T a b -> Maybe b
--lookup a (Dictionary dict) = Prelude.lookup a dict

--These are all the expressions we must consider in the value function:
  --Num Integer, Var String, Add Expr Expr, Sub Expr Expr, Mul Expr Expr,
  --Div Expr Expr

------------------------------------------------------------------------

value :: Expr -> Dictionary.T String Integer -> Integer
--If it's a number then just return it because it is the final value.
value (Num n) _ = n

value (Var v) dictionary = case (Dictionary.lookup v dictionary) of
  Nothing -> error ("Expr.value: undefined variable " ++ v)
  Just a -> a



value (Add exprl exprr) dictionary = value exprl dictionary + value exprr dictionary
value (Sub exprl exprr) dictionary = value exprl dictionary + value exprr dictionary
value (Mul exprl exprr) dictionary = value exprl dictionary * value exprr dictionary

--I tried to condense the repetitive stuff into the following keywords but the
--ghci thing didnt like it
  --where evaluateLeft = value exprl dictionary
        --evaluateRight = value exprr dictionary

value (Div exprl exprr) dictionary = case value exprr dictionary of
  0 -> error "Expr.value: Division by 0 not permitted"
  _ -> value exprl dictionary `div` value exprr dictionary



instance Parse Expr where
    parse = expr
    toString = shw 0
