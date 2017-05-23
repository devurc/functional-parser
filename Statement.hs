module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T |
    Comment String
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (variable, expression) = Assignment variable expression

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter statement #- require "end" >-> Begin

ifs = accept "if" -# Expr.parse # require "then" -# statement # require "else" -# statement >-> buildIf
buildIf ((expression, thenstmt), elsestmt) = If expression thenstmt elsestmt

while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
buildWhile(expression, statement) = While expression statement

readStatement = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

commentStatement = (accept "--" -#iter word) #- require "\n" >-> buildComment
buildComment s = Comment $ unwords s

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

exec (Assignment variable expression : statements) dict input = exec statements newDictEntry input
  where newDictEntry = Dictionary.insert (variable, Expr.value expression dict) dict

exec (Skip : statements) dict input = exec statements dict input

exec (Begin stmt : statements) dict input = exec newStatement dict input
  where newStatement = stmt ++ statements

exec (If cond thenStmts elseStmts: statements) dict input =
    if (Expr.value cond dict) > 0
    then exec (thenStmts : statements) dict input
    else exec (elseStmts : statements) dict input

exec (While expression stmt : statements) dict input
  | result > 0 = exec (stmt : whileStatement : statements) dict input
  | otherwise = skip
  where result = Expr.value expression dict
        whileStatement = While expression stmt
        skip = exec statements dict input

exec (Read variable : statements) dict (input:inputs) = exec statements newDictEntry inputs
  where newDictEntry = Dictionary.insert(variable, input) dict

exec (Write expression : statements) dict input = Expr.value expression dict : exec statements dict input

exec (Comment s : statements) dict input = exec statements dict input

statement = assignment ! skip ! begin ! ifs ! while ! readStatement ! write

indent = "  "

instance Parse Statement where
  parse = statement
  toString (Assignment variable expression) = variable ++ " := " ++ Expr.toString expression ++ ";" ++ "\n"
  toString (Read variable) = "read " ++ variable ++ ";" ++ "\n"
  toString (Write expression) = "write " ++ Expr.toString expression  ++ ";" ++ "\n"
  toString (Skip) = indent ++ "skip" ++ ";" ++ "\n"
  toString (While expression stmt) = "while " ++ Expr.toString expression ++ " do"
    ++ "\n" ++ indent ++ Expr.toString stmt

  toString (Begin statements) = "begin" ++ "\n" ++ stmts ++ indent ++ "end" ++ "\n"
    where stmts = foldr1 (++) $ map (doubleIndent ++) strings
          doubleIndent = indent ++ indent
          strings = map toString statements

  toString (If expression thenstmt elsestmt) = "if " ++ Expr.toString expression
    ++ " then" ++ "\n" ++ indent ++ Expr.toString thenstmt ++ indent
    ++ "else" ++ "\n" ++ indent ++ Expr.toString elsestmt ++ "\n"
