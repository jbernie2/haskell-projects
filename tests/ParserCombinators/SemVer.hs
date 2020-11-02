module ParserCombinators.SemVer where

import Control.Applicative
import Text.Trifecta
import Test.Hspec

data NumberOrString
  = NOSS String
  | NOSI Integer
  deriving (Show, Eq)

instance Ord NumberOrString where
  compare (NOSS str1) (NOSS str2) = compare str1 str2
  compare (NOSS str) (NOSI i) = compare str (show i)
  compare (NOSI i) (NOSS str) = compare (show i) str
  compare (NOSI i1) (NOSI i2) = compare i1 i2

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata
  deriving (Show, Eq)

instance Ord SemVer where
  compare 
    (SemVer major1 minor1 patch1 rel1 _) 
    (SemVer major2 minor2 patch2 rel2 _)  =
    getEquality [compareVersions, comparePreRelease]
   where
    getEquality :: [Ordering] -> Ordering
    getEquality xs = 
      case filter (/= EQ) xs of
        [] -> EQ
        (x : _) -> x

    compareVersions =
      getEquality $
        zipWith 
          compare
          [major1, minor1, patch1]
          [major2, minor2, patch2]

    comparePreRelease =
      case (length rel1 == 0, length rel2 == 0) of
        (True, False) -> GT
        (False, True) -> LT
        (True, True) -> EQ
        (False, False) ->
          if comparePreRelease' == EQ then
            compare (length rel1) (length rel2)  
          else
            comparePreRelease'

    comparePreRelease' =
      getEquality $ zipWith compare rel1 rel2
      
validChars :: [Char]
validChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890-"

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseVersions
  rel <- parseRelease
  metadata <- parseMetadata
  return (SemVer major minor patch rel metadata)

parseVersions :: Parser (Major, Minor, Patch)
parseVersions = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return (major, minor, patch)

parseRelease :: Parser Release
parseRelease = try $ do
  _ <- char '-'
  releaseNotes <- sepBy1 parseNumberOrString (symbol ".")
  return releaseNotes

parseMetadata :: Parser Metadata
parseMetadata = try $ do
  _ <- char '+'
  releaseNotes <- sepBy1 parseNumberOrString (symbol ".")
  return releaseNotes

parseNumberOrString :: Parser NumberOrString
parseNumberOrString =
      try (NOSI <$> integer <* notFollowedBy (oneOf validChars))
  <|> try (NOSS <$> some (oneOf validChars))
  

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "SemVer Parsing" $ do
    it "can parse SemVer String" $ do
      let m  = parseString parseSemVer mempty "1.4.11-some-release.123+notes-1.a"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ 
        SemVer
          1
          4
          11
          [ NOSS "some-release"
          , NOSI 123 ]
          [ NOSS "notes-1"
          , NOSS "a" ]
        )

  describe "Version Parsing" $
    it "can parse major minor and patch versions" $ do
      let m  = parseString parseVersions  mempty "123.456.789"
          r' = maybeSuccess m

      print m
      r' `shouldBe` Just (123, 456, 789)

  describe "Release Parsing" $
    it "can parse release notes" $ do
      let m  = parseString parseRelease mempty "-hello.123b.b123-7bf.123"
          r' = maybeSuccess m

      print m
      r' `shouldBe` (Just $
        [ NOSS "hello"
        , NOSS "123b"
        , NOSS "b123-7bf"
        , NOSI 123
        ])

  describe "MetaData Parsing" $
    it "can parse release notes" $ do
      let m  = parseString parseMetadata mempty "+hello.123b.b123-7bf.123"
          r' = maybeSuccess m

      print m
      r' `shouldBe` (Just $
        [ NOSS "hello"
        , NOSS "123b"
        , NOSS "b123-7bf"
        , NOSI 123
        ])

  describe "note parsing" $ do
    it "can parse a number" $ do
      let m  = parseString parseNumberOrString mempty "123"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ NOSI 123)

    it "can parse a string" $ do
      let m  = parseString parseNumberOrString mempty "abc-234"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ NOSS "abc-234")

    it "can parse a string that starts like a number" $ do
      let m  = parseString parseNumberOrString mempty "23b4-a"
          r' = maybeSuccess m
      print m
      r' `shouldBe` (Just $ NOSS "23b4-a")

  describe "SemVer Comparison" $ do
    describe "version comparison" $ do
      it "the larger major version is greater" $ do
        let big = SemVer 1 0 0 [] []
            small = SemVer 0 0 0 [] []
        (big > small) `shouldBe` True
        
      it "the larger minor version is greater when major is equal" $ do
        let big = SemVer 1 3 0 [] []
            small = SemVer 1 2 0 [] []
        (big > small) `shouldBe` True

      it "the larger patch version is greater when others are equal" $ do
        let big = SemVer 1 3 1 [] []
            small = SemVer 1 3 0 [] []
        (big > small) `shouldBe` True
