import Lib (simpleMathFunction)
import Test.Hspec
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    hspec simpleMathSpec
    defaultMain simpleMathTests

simpleMathTests :: TestTree
simpleMathTests =
    testGroup
        "Simple Math Tests"
        [ testCase "Small Numbers" $
            simpleMathFunction 3 4 5 @?= 7
        ]

simpleMathSpec :: Spec
simpleMathSpec = describe "Tests of our simple math function" $ do
    context "when the numbers are small" $
        it "Should match our expected value" $
            simpleMathFunction 3 4 5 `shouldBe` 7
    context "when the numbers are big" $
        it "Should match our expected value" $
            simpleMathFunction 22 12 64 `shouldBe` 200