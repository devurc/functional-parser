------------------------------------------------------------------------
--Contains a number of derived parsers and parser operators

------ ? is a condition checker
------ ! if the first one fails, do the second
------ # take the result of both parsers and smash into a pair
------ >-> LHS would be parser result and RHS would be a transformation
------ defining function.

------ The derived parsers appear here (shit is imported from CoreParser)
------------------------------------------------------------------------

module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
import Data.Maybe
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

--Cons = concatenate?
cons(a, b) = a:b

------------------------------------------------------------------------
--applies two parsers in sequence as # but throws away the result from the first one
--use # and >-> in the definition
--example: (char -# char) "abc" -> Just(’b’, "c")
--         (char -# char) "a" -> Nothing

--(#) means that the results of the two parsers are combined into a pair.

-- >-> is used to transform the result of a parser. The right operand of >-> named
-- k is the FUNCTION defining the transformation. The result of the operation is a
-- new parser.

--So make the two parsers into a pair and extract the first or the second.
------------------------------------------------------------------------

(-#) :: Parser a -> Parser b -> Parser b
m -# n = (m # n) >-> snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = (m # n) >-> fst

------------------------------------------------------------------------
--Gotta accept any number of whitespace characters as defined by isSpace
--Might need to use iter?
--iterate f x returns an infinite list of repeated applications of f to x
------------------------------------------------------------------------

spaces :: Parser String
spaces =  iter (char ? isSpace)

token :: Parser a -> Parser a
token m = m #- spaces

------------------------------------------------------------------------
--a parser for a letter as defined by the prelude function isAlpha
--Returns true if the character is an alphabetic letter
-- Char -> Bool
--the infix operator ? is a condition checker sorta

--As for word, this function parses a word by parsing first a letter, then by
--a sequence of letters while concatenating the resulting value.
------------------------------------------------------------------------

letter :: Parser Char
letter =  char ? isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

------------------------------------------------------------------------
--The parser chars n accepts n characters
--idk if this will work either
------------------------------------------------------------------------
chars :: Int -> Parser String
chars 0 = return []
chars n = char # chars (n-1) >-> cons


accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

------------------------------------------------------------------------
--my interpretation of accept w:
--Token is defined as ""token m = m #- spaces""
--Chars is defined as iterate Char n letter ==my definition so might not work==
--Takes in a string w
--Chars accepts the whole length of the string
--The token keyword strips the string of white spaces
--Then checks that this string satisfies the condition of being the same as the original w

--The parser require w accepts the same string as accept w but reports
--the missing string using err in case of failure. uh yeah so wtf
--this parser behaves as accept but emits an error message instead of
--returning nothing
--parses for a specific substring?

--err is defined as err message cs = error (message++" near "++cs++"\n")
--if accept w fails then continue to error message via (!)
------------------------------------------------------------------------

require :: String -> Parser String
require w  = accept w ! (err ("There is a missing string??"))

--Parsing for a specific character c
lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')
