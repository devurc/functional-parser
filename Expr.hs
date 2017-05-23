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

data Expr = Num Integer | Var String | Add Expr Expr
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
       | Pow Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr, pow :: Parser Expr

term', expr', pow' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> (\ _ -> Mul) !
        lit '/' >-> (\ _ -> Div)

addOp = lit '+' >-> (\ _ -> Add) !
        lit '-' >-> (\ _ -> Sub)

powOp = lit '^' >-> (\_ -> Pow)

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"

term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'

expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

pow' e = exOp # factor >-> bldOp e #> pow' ! return e
pow = factor #> pow'

parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n
shw prec (Var v) = v
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ " + " ++ shw 5 u)
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ " - " ++ shw 6 u)
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u)
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u)
shw prec (Pow t u) = parens (prec>7) (shw 7 t ++ "^" ++ shw 7 u)

value :: Expr -> Dictionary.T String Integer -> Integer
--If it's a number then just return it because it is the final value.
value (Num n) _ = n

value (Var v) dictionary = case (Dictionary.lookup v dictionary) of
  Nothing -> error ("Expr.value: undefined variable " ++ v)
  Just a -> a

value (Add exprl expr2) dictionary = value exprl dictionary + value expr2 dictionary
value (Sub exprl expr2) dictionary = value exprl dictionary - value expr2 dictionary
value (Mul exprl expr2) dictionary = value exprl dictionary * value expr2 dictionary
value (Div exprl expr2) dictionary = case value expr2 dictionary of
  0 -> error "Expr.value: Division by 0 not permitted"
  _ -> value exprl dictionary `div` value expr2 dictionary
value (Pow expr1 expr2) dictionary = (value expr1 dictionary) ^ (value expr2 dictionary)

instance Parse Expr where
    parse = expr
    toString = shw 0
