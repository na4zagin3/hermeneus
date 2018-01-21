module I18n.Hermeneus.MessageSpec where

import Text.Parsec

import Test.Tasty.HUnit

import I18n.Hermeneus.Prim
import I18n.Hermeneus.Message


unit_parseTranslationTemplate = do
  parse parseTranslationTemplate "" "abc" @?= (Right $ TranslatedString "abc")
  parse parseTranslationTemplate "" "{1}" @?= (Right $ Placeholder (PlaceholderNumber 1, []))
  parse parseTranslationTemplate "" "{0:number#2}" @?= (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr "number" $ ConcordWord $ PlaceholderNumber 2]))
  parse parseTranslationTemplate "" "{*to:number#2}" @?= (Right $ Placeholder (WordKey "to", [FeatureConstraintExpr "number" $ ConcordWord $ PlaceholderNumber 2]))
  parse parseTranslationTemplate "" "{0:number=singular}" @?= (Right $ Placeholder (PlaceholderNumber 0, [FeatureConstraintExpr "number" $ Feature "singular"]))


