{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Parser.Combinators
import Text.Trifecta (
    CharParsing (string),
    Parser (..),
    char,
    decimal,
    parseString,
 )

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'

-- Intermission: Exercises
-- 1. There’s a combinator that’ll let us mark that we expect
-- an input stream to be “finished” at a particular point in
-- our parser. In the parsers library this is simply called
-- ? eof (end-of-file) and is in the Text.Parser.Combinators module.
-- See if you can make the one and oneTwo parsers fail because
-- they didn’t exhaust the input stream!

oneH = char 'H'

justOneH = do oneH; eof

oneHello = string "Hello"

justOneHello = do oneHello; eof

testEx :: Show a => Parser a -> String -> IO ()
testEx p s = print $ parseString p mempty s

ex1 :: IO ()
ex1 = do
    line <- getLine
    testEx oneH line
    testEx justOneH line
    testEx oneHello line
    testEx justOneHello line

-- 2. Use string to make a Parser that parses “1”,“12”, and “123”
-- out of the example input respectively. Try combining it with
-- stop too.  That is, a single parser should be able to parse
-- all three of those strings.

string1 = string "1"

string1stop = do
    string1
    stop

ex2 :: IO ()
ex2 = do
    testEx string1 "1"
    testEx string1 "12"
    testEx string1 "123"

    testEx (string1stop :: Parser ()) "1"
    testEx (string1stop :: Parser ()) "12"
    testEx (string1stop :: Parser ()) "123"

-- 3. Try writing a Parser that does what string does, but using char
stringParser :: String -> Parser String
stringParser [] = return []
stringParser (x : xs) = do
    px <- char x
    rx <- stringParser xs
    return (px : rx)

-- 24.4 Parsing fractions
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)

fracMain :: IO ()
fracMain = do
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad
    print $ parseString parseFraction mempty badFraction

-- the division by 0 halts the whole program

virtuousFraction :: Parser Rational
virtuousFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

-- ! Here is our first explicit use of fail, which by historical
-- ! accident is part of the Monad typeclass.

testVirtuous :: IO ()
testVirtuous = do
    print $ parseString virtuousFraction mempty badFraction
    print $ parseString virtuousFraction mempty alsoBad
    print $ parseString virtuousFraction mempty shouldWork
    print $ parseString virtuousFraction mempty shouldAlsoWork

-- Now we have no bottom causing the program to halt and we get a
-- Failure value which explains the cause for the failure. Much better