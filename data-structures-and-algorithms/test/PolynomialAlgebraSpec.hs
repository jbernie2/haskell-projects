module PolynomialAlgebraSpec where

import Test.Hspec
import PolynomialAlgebra (Term(..), add, evaluate, times)

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can add polynomials" $ do
    evaluate 10 $ (Term 0 1.0 Nil) `add` (Term 0 2.0 Nil)
    `shouldBe`
    3

  it "can multiply polynomials" $ do
    evaluate 10 $
      (Term 0 1.0 (Term 0 2.0 Nil))
      `times`
      (Term 0 2.0 (Term 0 3.0 (Term 0 3.0 Nil)))
    `shouldBe`
    24
  
  it "can multiply polynomials" $ do
    evaluate 10 $
      (Term 2 1.0 (Term 3 2.0 Nil)) -- 1x^2 + 2x^3 == 100 + 2000 == 2100
      `times`
      (Term 0 2.0 (Term 2 3.0 (Term 1 3.0 Nil))) -- 2x^0 + 3x^2 + 3x^1 == 2 + 300 + 30 == 332
    `shouldBe`
    (2100 * 332)
    
