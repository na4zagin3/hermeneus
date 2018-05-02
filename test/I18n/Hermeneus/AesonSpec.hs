module I18n.Hermeneus.AesonSpec where

import Data.Aeson
import Text.Parsec
import I18n.Hermeneus.Prim

import Generic.Random
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import I18n.Hermeneus.ArbitraryInstances

import I18n.Hermeneus.Aeson

testIfParserSection :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
testIfParserSection expr = fromJSON (toJSON expr) == pure expr

prop_TranslationTemplate_parser_section :: TranslationTemplate -> Bool
prop_TranslationTemplate_parser_section = testIfParserSection

prop_NumberHandling_parser_section :: NumberHandling -> Bool
prop_NumberHandling_parser_section = testIfParserSection

prop_LocalizedWord_parser_section :: LocalizedWord -> Bool
prop_LocalizedWord_parser_section = testIfParserSection

prop_LangInfo_parser_section :: LangInfo -> Bool
prop_LangInfo_parser_section = testIfParserSection

prop_TranslationSet_parser_section :: TranslationSet -> Bool
prop_TranslationSet_parser_section = testIfParserSection
