------------------------------------------------------------------------
--This module contains the data type for representing a statement, a statement
--parser, a function to interpret a list of statements and a function for
--converting the representation to a string

--For the assignment: need to implement the type and functions
--The data type T should have 7 constructors, one for each kind of statement
--e.g. if then else, read, write etc

--Define a parsing function for each kind of statement.
--If the parser has accepted the first reserved word in a statement, use require
--rather than accept to parse other reserved words or symbols in order to get
--better error messages in case of failure.
------------------------------------------------------------------------

module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Begin [statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Read String |
    Write Expr.T
    deriving Show

----PARSING FUNCTIONS----
--Type Signature--
assignment, skip, begin, if, while, read, write :: Parser Statement

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (variable, expression) = Assignment variable expression

skip = accept "skip" # require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter statement #- require "end" >-> Begin

if = accept "if" -# Expr.parse #- require "then" statement #- require "else" # statement >-> buildIf
buildIf ((expression, thenstmt), elsestmt) = If expression thenstmt elsestmt

while = accept "while" -# Expr.parse #- require "do" #statement >-> buildWhile
buildWhile(expression, statement) = While expression statement

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

------------------------------------------------------------------------
--Use the above statements to define the parse function, put all statements under
--a new variable, 'statement'
--Also need to implement 'toString'. Has data type of toString :: a -> String
--Indentation needed?

--The exec function has type signature: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
--takes a list of statements to be executed ([T]), a dictionary containing variable/value
--pairs (Dictionary.T String Integer) and a list of integers containing numbers that may be read
--by 'read' statements ([integer])
--The returned list contains the numbers produced by the write statements ([Integer]). This function
--is defined by using pattern matching on the first argument

--Also statements are of type T. Recursively call exec while updating dictionary and inputs where needed
------------------------------------------------------------------------

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

--The dictionary insert function takes in two (tuple) values, the key and value pair.
exec (Assignment variable expression : statements) dict input = exec statements newDictEntry input
  where newDictEntry = (Dictionary.insert (variable, Expr.value expression dict) dict)

--Skipping. Recursively calling on statements (that follow after the skip keyword in program) with dict and input
exec (Skip : statements) dict input = exec statements dict input

exec (Begin stmt : statements) dict input = exec (stmt ++ statements) dict input

exec (If cond thenStmts elseStmts: statements) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: statements) dict input
    else exec (elseStmts: statements) dict

--Similar to the 'If Then Else' exec, if the while condition is satisfied, then the 'then' instruction is
--executed. Else, just skip instead of a condition
exec (While expression stmt: statements) =
  if (Expr.value cond dict)>0
  then exec (stmt:(While expression stmt):statements) dict input
  else exec statements dict inpit

exec (Read variable : statements) dict (input:inputs) = exec statements (Dictionary.insert(variable, input) dict) inputs

exec (write expression : statements) dict input = Expr.value expression dict : exec statements dict input

statement = assignment ! skip ! begin ! if ! while ! read ! write

instance Parse Statement where
  parse = statement
  toString (Assignment variable expression) = variable ++ ":=" ++ Expr.toString expression ++ ";" ++ "\n""
  toString (Skip) = "skip" ++ ";" ++ "\n""
  toString (Begin statements) = "begin" ++ "\n"" ++ stmt ++ "end" ++ "\n""
    where stmt = foldr (++) $ map toString statements
  toString (If expression thenstmt elsestmt) = "if" ++ Expr.toString expression ++ "then" ++ "\n"" ++ Expr.toString thenstmt ++ "else" ++ Expr.toString elsestmt ++ "\n""
  toString (While expression stmt) = "while" ++ expression ++ "do" ++ "\n"" ++ Expr.toString stmt
  toString (Read variable) = "read" ++ variable ++ ";" ++ "\n""
  toString (Write expression) = "write" ++ Expr.toString expression  ++ ";" ++ "\n""
