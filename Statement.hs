--------------------------------------------------------------------------------
-- This module contains a:
--    datatype for representing a statement
--    a statement parser
--    function to interpret a list of statements
--    function for converting the representation into a string
--------------------------------------------------------------------------------

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

statementList = assignmentstmt ! skipstmt ! beginstmt ! ifstmt ! whilestmt ! readstmt ! writestmt ! commentstmt

assignmentstmt = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skipstmt = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

beginstmt = accept "begin" -# iter statementList #- require "end" >-> buildBegin
buildBegin (ss) = Begin ss

ifstmt = accept "if" -# Expr.parse # require "then" -# statementList # require "else" -# statementList >-> buildIf
buildIf ((e, ts), es) = If e ts es

whilestmt = accept "while" -# Expr.parse #- require "do" # statementList >-> buildWhile
buildWhile(e, s) = While e s

readstmt = accept "read" -# word #- require ";" >-> buildRead
buildRead v = Read v

writestmt = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e

commentstmt = (accept "--" -# iter word) #- require "\\n" >-> buildComment
buildComment c = Comment (unwords c)

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []

--In the following lines of code we have shortened keywords for easier readability:
-- v = variable
-- e = expression
-- s = statement
-- ss = statements
-- ts = then statement
-- es = else statement
-- d = dict
-- i = input

exec (Assignment v e : s) d i = exec s newDictEntry i
  where newDictEntry = Dictionary.insert (v, Expr.value e d) d

exec (Skip : s) d i = exec s d i

exec (Begin s : ss) d i = exec newStatement d i
  where newStatement = s ++ ss

exec (If cond ts es : s) d i =
    if (Expr.value cond d) > 0
    then exec (ts : s) d i
    else exec (es : s) d i

exec (While e s : ss) d i
  | result > 0 = exec (s : whileStatement : ss) d i
  | otherwise = skip
  where result = Expr.value e d
        whileStatement = While e s
        skip = exec ss d i

exec (Read v : ss) d (i : is) = exec ss newDictEntry is
  where newDictEntry = Dictionary.insert(v, i) d

exec (Write e : ss) d i = Expr.value e d : exec ss d i

exec (Comment c : s) d i = exec s d i

indent = "  "

instance Parse Statement where
  parse = statementList

  toString (Assignment variable expression) = variable ++ " := " ++ Expr.toString expression ++ ";" ++ "\n"

  toString (Read variable) = "read " ++ variable ++ ";" ++ "\n"

  toString (Write expression) = "write " ++ Expr.toString expression  ++ ";" ++ "\n"

  toString (Skip) = indent ++ "skip" ++ ";" ++ "\n"

  toString (While expression statement) = "while " ++ Expr.toString expression ++ " do"
    ++ "\n" ++ indent ++ Expr.toString statement

  toString (Begin statements) = "begin" ++ "\n" ++ stmts ++ indent ++ "end" ++ "\n"
    where stmts = foldr1 (++) $ map (doubleIndent ++) strings
          doubleIndent = indent ++ indent
          strings = map toString statements

  toString (If expression thenstmt elsestmt) = "if " ++ Expr.toString expression
    ++ " then" ++ "\n" ++ indent ++ Expr.toString thenstmt ++ indent
    ++ "else" ++ "\n" ++ indent ++ Expr.toString elsestmt

  toString (Comment c) = "-- " ++ c ++ "\\n" ++ "\n"
