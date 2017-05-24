--------------------------------------------------------------------------------
-- This module contains a:
--     data type for representing a program
--     program parser
--     program interpreter
--     function for converting the representation into a string
--------------------------------------------------------------------------------

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
