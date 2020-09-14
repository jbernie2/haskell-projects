module Dog where

import Readers.Reader

newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)
  
getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Reader $ Dog <$> dogName <*> address

  
