module Main where

import Text.Parser.Combinators
import Text.Trifecta (CharParsing (string), Parser (..), char, parseString)

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
