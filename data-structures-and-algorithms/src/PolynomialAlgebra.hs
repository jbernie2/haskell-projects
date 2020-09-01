module PolynomialAlgebra where

data Term
  = Term Int Double Term
  | Nil
  deriving (Show)

evaluate :: Integer -> Term -> Double 
evaluate base (Term degree coefficient term) =
  coefficient
  * (fromIntegral base)
  ^ degree
  + (evaluate base term)
evaluate _ Nil = 0

add :: Term -> Term -> Term
add Nil term = term
add term Nil = term
add (Term d c term) term' = Term d c (add term term')

times :: Term -> Term -> Term
times Nil _ = Nil
times _ Nil = Nil
times (Term d c term) (Term d' c' term') =
  add
    (add
      (Term (d + d') (c * c') Nil)
      (times (Term d c Nil) term')
    )
    (times term (Term d' c' term'))

