{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Monoid (Any)
import Data.Ratio ((%))
import Text.Parser.Combinators
import Text.RawString.QQ (r)
import Text.Trifecta (
    CharParsing (string),
    Parser (..),
    char,
    decimal,
    digit,
    integer,
    letter,
    oneOf,
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

-- Intermission: Exercise

-- This should not be unfamiliar at this point, even if
-- you do not understand all the details:

-- Prelude> parseString integer mempty "123abc"
-- Success 123
-- Prelude> parseString (integer >> eof) mempty "123abc"
-- Failure (interactive):1:4: error: expected: digit,
-- end of input
--  123abc<EOF>
--  ^
-- Prelude> parseString (integer >> eof) mempty "123"
-- Success ()

-- You may have already deduced why it returns () as a Success result
-- here; it’s consumed all the input but there is no result to return from
-- having done so. The result Success () tells you the parse was
-- successful and consumed the entire input, so there’s nothing to return.
-- What we want you to try now is rewriting the final example so it returns
-- the integer that it parsed instead of Success (). It should return the
-- integer successfully when it receives an input with an integer followed
-- by an EOF and fail in all other cases:

justInteger :: Parser Integer
justInteger = integer >>= (eof >>) . return

-- justInteger = do
--     i <- integer
--     eof
--     return i

-- Prelude> parseString (justInteger) mempty "123"
-- Success 123
-- Prelude> parseString (justInteger) mempty "123abc"
-- Failure (interactive):1:4: error: expected: digit,
-- end of input
-- 123abc<EOF>

-- 24.5 Haskell's parsing ecosystem

-- 25.6 Alternative

type NumberOrString = Either Integer String

parseNos :: Parser NumberOrString
parseNos =
    (Left <$> integer) <|> (Right <$> some letter)

alternativeTest :: IO ()
alternativeTest = do
    let a = "blah"
        b = "123"
        c = "123blah789"
    print $ parseString (some letter) mempty a
    print $ parseString integer mempty b
    print $ parseString parseNos mempty a
    print $ parseString parseNos mempty b
    print $ parseString (many parseNos) mempty c
    print $ parseString (some parseNos) mempty c

-- What we’re taking advantage of here with some, many,
-- and (<|>) is the Alternative typeclass:
{-
class Applicative f => Alternative f where
    -- The identity of '<|>'
    empty :: f a

    -- An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- One or more
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (fmap (:) v) <*> many_v

    -- Zero or more
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (fmap (:) v) <*> many_v
-}

eitherOr :: String
eitherOr =
    [r|
123
abc
456
def
|]

-- QuasiQuotes
-- [r| is beginning of a quasiquoted section
-- It is defined a follows:
{-
r :: QuasiQuoter
r = QuasiQuoter {
    -- Extracted from dead-simple-json.
    quoteExp  = return . LitE . StringL . normaliseNewlines,

    -- error messages elided
    quotePat  = \_ -> fail "some error message",
    quoteType = \_ -> fail "some error message",
    quoteDec  = \_ -> fail "some error message"
}
-}

-- ? The idea here is that this is a macro that lets us write
-- ? arbitrary text inside of the block that begins with
-- ? [r| and ends with |].

-- Return to Alternative

mainAlt :: IO ()
mainAlt = do
    print $ parseString parseNos mempty eitherOr

parseNos' :: Parser NumberOrString
parseNos' = do
    skipMany (oneOf "\n")
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v

mainAltNL :: IO ()
mainAltNL = do
    print $ parseString (some parseNos') mempty eitherOr

-- Intermission: Exercise
-- Make a parser, using the existing fraction parser plus a new
-- ! decimal parser, that can parse either decimals or fractions.
-- You’ll want to use <|> from Alternative to combine the…alternative
-- parsers. If you find this too difficult, write a parser that
-- parses straightforward integers or fractions. Make a datatype
-- that contains either an integer or a rational and use that
-- datatype as the result of the parser. Or use Either. Run
-- free, grasshopper.
-- ? Hint: we’ve not explained it yet, but you may want to try `try`.

type Decimal = String

badDecimal = "1."
alsoBadDecimal = "1"
shouldWorkDecimal = "1.0"
shouldAlsoWorkDecimal = "3.14"

parseDecimal :: Parser Decimal
parseDecimal = do
    s <- some digit
    char '.'
    t <- some digit
    eof
    return $ s ++ "." ++ t

testDecimal :: IO ()
testDecimal = do
    print $ parseString parseDecimal mempty badDecimal
    print $ parseString parseDecimal mempty alsoBadDecimal
    print $ parseString parseDecimal mempty shouldWorkDecimal
    print $ parseString parseDecimal mempty shouldAlsoWorkDecimal

type DecimalOrRational = Either Decimal Rational

parseDor :: Parser DecimalOrRational
parseDor = (Left <$> try parseDecimal) <|> (Right <$> parseFraction)

testDor :: IO ()
testDor = do
    let a = "1.0"
        b = "1/2"
        c = "3.14"
        d = "3.14.0"
    print $ parseString parseDor mempty a
    print $ parseString parseDor mempty b
    print $ parseString parseDor mempty c
    print $ parseString parseDor mempty d
