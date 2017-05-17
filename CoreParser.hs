------------------------------------------------------------------------
--Defines the Parser type and implements the 3 elementary parsers, char, return and fail.
--Also basic parser operators.
--Class Parse with signatures for parse, toString and fromString
--Implementation for the last one is introduced??
--The representation of the Parser type is visible outside the module, but
--this visibility should not be exploited

-- FIRST PARSER MODULE --
-- This module should contain the:
-- - parser type
-- - basic parsers
-- - parser operators

-- the representation of parsers as values of type String -> Maybe(a, String)
-- will not be exploited outside this module

-- is useful to have a type class declaring the names parse and toString and
-- defining fromString
-- We then can use these names for all the datatypes. **Parse** is the name of this
-- type class.
------------------------------------------------------------------------

module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),
                  Parse, parse, toString, fromString) where
import Prelude hiding (return, fail)
infixl 3 !
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

type Parser a = String -> Maybe (a, String)


----CHAR PARSER----
--Basic parser that will accept only one character
--Synonym for String -> Maybe(Char, String)
--Function will fail if the input string is empty

char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

----RETURN PARSER----
--Return will always succeed without inspecting the input string.
--It returns the first argument which may be of any type.

return :: a -> Parser a
return a cs = Just (a, cs)

----FAIL PARSER----
--This parser will never accept any input. Fail is hidden when importing prelude.

fail ::  Parser a
fail cs = Nothing

------PARSER OPERATORS-------

----ALTERNATIVES IN GRAMMAR----
--(!) is an infix operator
--example: (m ! n)
--If m is applied to the input string and succeeds then thats it
--If it fails then apply n to input string
--AKA try m first then n

(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs
             mcs -> mcs

--(?) is also an infix operator
--example: (m ? p) is a parser which applies m to an input string and tests
--if the result satisfies p

(?) :: Parser a -> (a -> Bool) -> Parser a
(m ? p) cs =
    case m cs of
    Nothing -> Nothing
    Just(r, s) -> if p r then Just(r, s) else Nothing

--Applies two parsers in sequence where the remainder string from the first
--one is fed into the other
--The results of the two parsers are combined into a pair.

(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs =
    case m cs of
    Nothing -> Nothing
    Just(a, cs') ->
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

--Transforming the result of the parser. The right operand of >-> named k is
--the function defining the transformation. The result of the operation is a
--new parser.
--We may use it to define a parser which accepts a digit and returns an Int
--using digitToInt from the prelude

(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs =
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

--(#-) only will apply two parsers in sequence as # but throws away the result
--from the second one

--But sometimes we need to make the result from one parser available to another.
--After applying the parser p to the input string, both the result and the
--remainder input string are given to the second operand - k

(#>) :: Parser a -> (a -> Parser b) -> Parser b
(p #> k) cs =
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'
