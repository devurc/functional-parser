-----------------------------------------------------------------------
--This contains a data type for representing a program, a program parser,
--a program interpreter and a function for converting the representation
--into a string

--Assignment: represent the program as a statement list.
--Use parse function from statement module
--Use exec function from statement module

--Implement toString :: T -> String. A newline character should be inserted after
--each statement and some keywords. Use indentation
-----------------------------------------------------------------------

module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

newtype T = Program [Statement.T]

instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program statements)= concat $ map Statement.toString statements

exec (Program statements) input = Statement.exec statements Dictionary.empty input
