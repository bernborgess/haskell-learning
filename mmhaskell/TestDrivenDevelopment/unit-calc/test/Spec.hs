import Lib (simpleMathFunction)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain simpleMathTests

simpleMathTests :: TestTree
simpleMathTests =
    testGroup
        "Simple Math Tests"
        [ testCase "Small Numbers" $
            simpleMathFunction 3 4 7 @?= 7
        ]