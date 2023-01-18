module Main where

import qualified Data.Map as M
import Test.Tasty
import Test.Tasty.HUnit

import WorkbookQuestions

main :: IO ()
main = defaultMain $ testGroup "Workbook Tests"
  [ evensTests
  , addWhenMod3Is2Tests 
  , reverseTests 
  , reverseAccumTests 
  , specialMultiplesTests 
  , manyStringsTests 
  , addPairsTests 
  , listToMapTests 
  , sumWithParityTests 
  , jumpingStairsTests ]

testEvens :: (Eq a, Show a) => Int -> [a] -> [a] -> TestTree
testEvens i input expected = testCase ("Evens " ++ show i) $
  evens input @?= expected

evensTests :: TestTree
evensTests = testGroup "Evens Tests"
  [ testEvens 1 ([] :: [Int]) []
  , testEvens 2 [1] [] 
  , testEvens 3 ["hi", "bye"] ["bye"] 
  , testEvens 4 [1,2,3] [2]
  , testEvens 5 [1,2,3,4] [2,4]
  , testEvens 6 [1,2,3,4,5,6,7] [2,4,6] ]

testAddWhenMod3Is2 :: Int -> [Int] -> [Int] -> TestTree
testAddWhenMod3Is2 i input expected = testCase ("AddWhenMod3Is2 " ++ show i) $
  addWhenMod3Is2 input @?= expected

addWhenMod3Is2Tests :: TestTree
addWhenMod3Is2Tests = testGroup "AddWhenMod3Is2 Tests"
  [ testAddWhenMod3Is2 1 [] []
  , testAddWhenMod3Is2 2 [2] [5]
  , testAddWhenMod3Is2 3 [2,4,5,8,9] [5,8,11] ]

testReverse :: (Eq a, Show a) => Int -> [a] -> [a] -> TestTree
testReverse i input expected = testCase ("Reverse " ++ show i) $
  reverse_ input @?= expected

reverseTests :: TestTree
reverseTests = testGroup "Reverse Tests"
  [ testReverse 1 ([] :: [Int]) []
  , testReverse 2 [1] [1]
  , testReverse 3 ["hi", "yo", "bye"] ["bye", "yo", "hi"] ]

testReverseAccum :: (Eq a, Show a) => Int -> [a] -> [a] -> TestTree
testReverseAccum i input expected = testCase ("ReverseAccum " ++ show i) $
  reverseAccum input @?= expected

reverseAccumTests :: TestTree
reverseAccumTests = testGroup "ReverseAccum Tests"
  [ testReverseAccum 1 ([] :: [Int]) []
  , testReverseAccum 2 [1] [1]
  , testReverseAccum 3 ["hi", "yo", "bye"] ["bye", "yo", "hi"] ]

testSpecialMultiples :: Int -> [Int] -> [Int] -> TestTree
testSpecialMultiples i input expected = testCase ("SpecialMultiples " ++ show i) $
  specialMultiples input @?= expected

specialMultiplesTests :: TestTree
specialMultiplesTests = testGroup "SpecialMultiples Tests"
  [ testSpecialMultiples 1 [1] [2,3,4]
  , testSpecialMultiples 2 [1,2] [2,3,4,4,6,8]
  , testSpecialMultiples 3 [2,5,8] [4,6,8,10,15,20,16,24,32] ]

testManyStrings :: Int -> ([Int], [String]) -> [String] -> TestTree
testManyStrings i (inputInts, inputStrs) expected = testCase ("ManyStrings " ++ show i) $
  manyStrings inputInts inputStrs @?= expected

manyStringsTests :: TestTree
manyStringsTests = testGroup "ManyStrings Tests"
  [ testManyStrings 1 ([], ["hi"]) []
  , testManyStrings 2 ([1], []) []
  , testManyStrings 3 ([2,3], ["hi", "bye"]) ["hi", "hi", "bye", "bye", "bye"]
  , testManyStrings 4 ([4], ["yo", "you"]) ["yo", "yo", "yo", "yo"] ]


testAddPairs :: Int -> [Int] -> [Int] -> TestTree
testAddPairs i input expected = testCase ("AddPairs " ++ show i) $
  addPairs input @?= expected

addPairsTests :: TestTree
addPairsTests = testGroup "AddPairs Tests"
  [ testAddPairs 1 [] []
  , testAddPairs 2 [1] []
  , testAddPairs 3 [1,2,3,4] [3,7]
  , testAddPairs 4 [1,3,5,7,9,11,13] [4,12,20] ]

testListToMap :: Int -> [Int] -> M.Map String Int -> TestTree
testListToMap i input expected = testCase ("ListToMap " ++ show i) $
  listToMap input @?= expected

listToMapTests :: TestTree
listToMapTests = testGroup "ListToMap Tests"
  [ testListToMap 1 [] (M.fromList [])
  , testListToMap 2 [1,2,3] (M.fromList [("1", 1), ("2", 2), ("3", 3)])
  , testListToMap 3 [11, 5] (M.fromList [("5", 5), ("11", 11)]) ]

testSumWithParity :: Int -> [Int] -> Int -> TestTree
testSumWithParity i input expected = testCase ("SumWithParity " ++ show i) $
  sumWithParity input @?= expected

sumWithParityTests :: TestTree
sumWithParityTests = testGroup "SumWithParity Tests"
  [ testSumWithParity 1 [] 0
  , testSumWithParity 2 [2,4,6] 32
  , testSumWithParity 3 [1,2,3,4,5] 39 ]

testJumpingStairs :: Int -> ([Int], [(String,Int)]) -> ([String], [String]) -> TestTree
testJumpingStairs i (inputInts, inputTuples) expected = testCase ("JumpingStairs " ++ show i) $
  jumpingStairs inputInts inputTuples @?= expected

jumpingStairsTests :: TestTree
jumpingStairsTests = testGroup "JumpingStairs Tests"
  [ testJumpingStairs 1 ([2,1,3], [("First", 4)]) ([], ["First"])
  , testJumpingStairs 2 ([2,3,5,6,7], [("First", 4)]) (["First"], [])
  , testJumpingStairs 3 ([6,4,10,3], [("First", 5), ("Second", 5), ("Third", 4), ("Fourth", 5)])
    (["First", "Second", "Third"], ["Fourth"]) ]
