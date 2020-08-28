module MismatchedParens where

mp :: String -> Int
mp str =
  case toParens str of
    Just parens ->
      mp' [] parens
    Nothing ->
      -1

mp' :: [Paren] -> [Paren] -> Int
mp' stack (LP : parens) = mp' (LP : stack) parens
mp' (LP : stack) (RP : parens) = mp' stack parens
mp' (RP : stack) (RP : parens) = mp' stack parens
mp' [] (RP : parens) = 1 + (mp' [] parens)
mp' stack [] = length stack

data Paren = LP | RP deriving (Show, Eq)

toParens :: String -> Maybe [Paren]
toParens str =
  foldr
    (\c mList->
      case (c, mList) of
        ('(', Just list) -> Just $ LP : list
        (')', Just list) -> Just $ RP : list
        (_, _) -> Nothing
    )
    (Just [])
    str
        
        
