module I18n.Hermeneus.MessageSpec where

import Text.Parsec

import Test.Tasty.HUnit

import I18n.Hermeneus.Prim
import I18n.Hermeneus.Message

{-# ANN module "HLint: ignore Use camelCase" #-}

unit_parseTranslationHank = do
  parse parseTranslationHank "" "abc" @?= (Right $ TranslatedString "abc")
  parse parseTranslationHank "" "a{{b" @?= (Right $ TranslatedString "a{b")
  parse parseTranslationHank "" "{{" @?= (Right $ TranslatedString "{")
  parse parseTranslationHank "" "{{1" @?= (Right $ TranslatedString "{1")
  parse parseTranslationHank "" "{{1}}" @?= (Right $ TranslatedString "{1}")
  parse parseTranslationHank "" "1}}" @?= (Right $ TranslatedString "1}")
  parse parseTranslationHank "" "{1}" @?= (Right $ Placeholder (PlaceholderNumber 1, []))
  parse parseTranslationHank "" "{0:number#2}" @?= (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr "number" $ ConcordWord $ PlaceholderNumber 2]))
  parse parseTranslationHank "" "{*to:number#2}" @?= (Right $ Placeholder (WordKey "to", [FeatureConstraintExpr "number" $ ConcordWord $ PlaceholderNumber 2]))
  parse parseTranslationHank "" "{0:number=singular}" @?= (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr "number" $ Feature "singular"]))


unit_parseTranslationTemplate = do
  parse parseTranslationTemplate "" "" @?= (Right $ TranslationTemplate [])

  parse parseTranslationTemplate "" "abc {0} by {1:number=singular}" @?=
    (Right $ TranslationTemplate
      [ TranslatedString "abc "
      , Placeholder
         (PlaceholderNumber 0, [])
      , TranslatedString " by "
      , Placeholder
         (PlaceholderNumber 1,
          [FeatureConstraintExpr "number" $ Feature "singular"])
      ])
