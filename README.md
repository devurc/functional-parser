# functional-parser

# Summary
This is a parser and an interpreter for a small imperative language. The main goal was to get acquainted with a monadic way of solving a classical problem in computer science.

# Introduction
The following program is written in the programming language to be parsed and interpreted in this assignment.

   read k;
   read n;
   m := 1;
   while n-m do
     begin
       if m - m/k*k then
         skip;
       else
         write m;
       m := m + 1;
     end
The language has just one data type, integer, and variables are not declared. In the while and if statements a positive expression value is interpreted as true while 0 and negative values mean false.

The program above reads two integers, k and n, and writes all integers between 1 and n that are multiples of k.

The grammar for the language is given by

   program ::= statements
   statement ::= variable ':=' expr ';'
           | 'skip' ';'
           | 'begin' statements 'end'
           | 'if' expr 'then' statement 'else' statement
           | 'while' expr 'do' statement
           | 'read' variable ';'
           | 'write' expr ';'
   statements ::= {statement}
   variable ::= letter {letter}
An explanation of this grammar and a parser for an expression, expr, can be found in the document Parsing with Haskell, by Lennart Andersson. The intended semantics for the language should be obvious from the keywords for anybody familiar with Java language.

# Program structure
You are given the stub of a solution:

CoreParser.hs
defines the Parser type and implements the three elementary parsers, char, return and fail, and the basic parser operators #, !, ?, #>, and >->, described in Lennart Andersson's description (and during the lecture).

The class Parse with signatures for parse, toString, and fromString with an implementation for the last one is introduced.

The representation of the Parser type is visible outside the module, but this visibilty should not be exploited.

Parser.hs
contains a number of derived parsers and parser operators.

Expr.hs
contains a data type for representing an arithmetic expression, an expression parser, an expression evaluator, and a function for converting the representation to a string.

Dictionary.hs
contains a data type for representing a dictionary.

Statement.hs
contains a data type for representing a statement, a statement parser, a function to interpret a list of statements, and a function for converting the representation to a string.

Program.hs
contains a data type for representing a program, a program parser, a program interpreter, and a function for converting the representation to a string.

Test*.hs
contain test data.

In a test using the program in the introduction with the following definitions

   src = "read k; read n; m:=1; ... "
   p = Program.fromString src
the expression Program.exec p [3,16] should return [3,6,9,12,15].
