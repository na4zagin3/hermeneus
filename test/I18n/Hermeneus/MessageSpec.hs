{-# LANGUAGE DeriveGeneric #-}

module I18n.Hermeneus.MessageSpec where

import Data.Either
import Data.Monoid
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)
import Text.Parsec

import Generic.Random
import Test.Tasty.HUnit
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import qualified Test.QuickCheck.Unicode as QU
import I18n.Hermeneus.ArbitraryInstances

import I18n.Hermeneus.Prim
import I18n.Hermeneus.Message
import GHC.Generics (Generic, Rep)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

translatedString = TranslatedString . nonEmptyStringFromString

spec_parseTranslationHank = describe "parseTranslationHank" $ do
  it "does not parse a empty string" $
    isLeft (parse parseTranslationHank "" "") `shouldBe` True
  it "parses a normal translated string" $
    parse parseTranslationHank "" "abc" `shouldBe` (Right $ translatedString "abc")
  it "parses a translated string with escaped braces" $ do
    parse parseTranslationHank "" "a{{b" `shouldBe` (Right $ translatedString "a{b")
    parse parseTranslationHank "" "{{" `shouldBe` (Right $ translatedString "{")
    parse parseTranslationHank "" "{{1" `shouldBe` (Right $ translatedString "{1")
    parse parseTranslationHank "" "{{1}}" `shouldBe` (Right $ translatedString "{1}")
    parse parseTranslationHank "" "1}}" `shouldBe` (Right $ translatedString "1}")
  it "parses a placeholder with reference number" $
    parse parseTranslationHank "" "{1}" `shouldBe` (Right $ Placeholder (PlaceholderNumber 1, []))
  it "parses a placeholder with reference number with empty agreement" $
    parse parseTranslationHank "" "{0:}" `shouldBe` (Right $ Placeholder (PlaceholderNumber 0, []))
  it "parses a placeholder with reference number with argument-agreement" $
    parse parseTranslationHank "" "{0:number#2}" `shouldBe` (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ ConcordWord $ PlaceholderNumber 2]))
  it "parses a placeholder with empty reference word" $
    parse parseTranslationHank "" "{**}" `shouldBe` (Right $ Placeholder (WordKey "", []))
  it "parses a placeholder with empty reference word and a colon" $
    parse parseTranslationHank "" "{**:}" `shouldBe` (Right $ Placeholder (WordKey "", []))
  it "parses a placeholder with reference word ':'" $
    parse parseTranslationHank "" "{*:*}" `shouldBe` (Right $ Placeholder (WordKey ":", []))
  it "parses a placeholder with reference word ':' with a colon" $
    parse parseTranslationHank "" "{*:*:}" `shouldBe` (Right $ Placeholder (WordKey ":", []))
  it "does not parse '{***}'" $
    isLeft (parse parseTranslationHank "" "{***}") `shouldBe` True
  it "parses a placeholder with reference word '*'" $
    parse parseTranslationHank "" "{****}" `shouldBe` (Right $ Placeholder (WordKey "*", []))
  it "parses a placeholder with reference word with argument-agreement" $
    parse parseTranslationHank "" "{*to*:number#2}" `shouldBe` (Right $ Placeholder (WordKey "to", [FeatureConstraintExpr numberFeature $ ConcordWord $ PlaceholderNumber 2]))
  it "parses a placeholder with reference word with literal-agreement" $
    parse parseTranslationHank "" "{0:number=singular}" `shouldBe` (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ Feature singularValue]))
  it "parses a placeholder with reference word with multiple literal-agreements" $
    parse parseTranslationHank "" "{0:number=singular,gender=feminine}" `shouldBe` (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ Feature singularValue, FeatureConstraintExpr genderFeature $ Feature feminineValue ]))
  it "does not parse a illegal empty placeholder" $ do
    isLeft (parse parseTranslationHank "" "{}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{:}") `shouldBe` True
  it "does not parse a illegal placeholder with negative reference number" $
    isLeft (parse parseTranslationHank "" "{-1}") `shouldBe` True
  it "does not parse a illegal placeholder with illegal agreements" $ do
    isLeft (parse parseTranslationHank "" "{:*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{:*#0}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{:#0*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{:a#0*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{:==0*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{0:*#0}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{0:#0*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{0:a#0*}") `shouldBe` True
    isLeft (parse parseTranslationHank "" "{0:==0*}") `shouldBe` True


unit_parseTranslationTemplate = do
  parse parseTranslationTemplate "" "" @?= (Right $ TranslationTemplate [])

  parse parseTranslationTemplate "" "{{{1}" @?= (Right $ TranslationTemplate [translatedString "{", Placeholder (PlaceholderNumber 1, [])])

  parse parseTranslationTemplate "" "abc {0} by {1:number=singular}" @?=
    (Right $ TranslationTemplate
      [ translatedString "abc "
      , Placeholder
         (PlaceholderNumber 0, [])
      , translatedString " by "
      , Placeholder
         (PlaceholderNumber 1,
          [FeatureConstraintExpr numberFeature $ Feature singularValue])
      ])

unit_printTranslationHank = do
  printTranslationHank (translatedString "abc") @?= "abc"
  printTranslationHank (translatedString "{") @?= "{{"
  printTranslationHank (translatedString "a{c") @?= "a{{c"
  printTranslationHank (translatedString "a{c}") @?= "a{{c}}"
  printTranslationHank (Placeholder (PlaceholderNumber 1, [])) @?= "{1}"
  printTranslationHank (Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ ConcordWord $ PlaceholderNumber 2])) @?= "{0:number#2}"
  printTranslationHank (Placeholder (WordKey "to", [FeatureConstraintExpr numberFeature $ ConcordWord $ PlaceholderNumber 2])) @?= "{*to*:number#2}"
  printTranslationHank (Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ Feature singularValue])) @?= "{0:number=singular}"
  printTranslationHank (Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr numberFeature $ Feature singularValue, FeatureConstraintExpr genderFeature $ Feature feminineValue])) @?= "{0:number=singular,gender=feminine}"


testIfParserSection name parser printer expr = parse parser ("parse " <> name) (printer expr :: Text) == Right expr

prop_FeatureReferenceExpr_parser_section :: FeatureReferenceExpr -> Bool
prop_FeatureReferenceExpr_parser_section = testIfParserSection "FeatureReferenceExpr" parseFeatureReferenceExpr printFeatureReferenceExpr

prop_WordReference_parser_section :: WordReference -> Bool
prop_WordReference_parser_section = testIfParserSection "WordReference" parseWordReference printWordReference

prop_TranslationHank_parser_section :: TranslationHank -> Bool
prop_TranslationHank_parser_section = testIfParserSection "TranslationHank" parseTranslationHank printTranslationHank

prop_TranslationTemplate_parser_section :: TranslationTemplate -> Bool
prop_TranslationTemplate_parser_section = testIfParserSection "TranslationTemplate" parseTranslationTemplate printTranslationTemplate . normalizeTranslationTemplate
