module I18n.Hermeneus.WordSpec where

import qualified Data.Map as M
import Data.Text (Text)
import Text.Parsec

import I18n.Hermeneus.Prim
import I18n.Hermeneus.Word

import Generic.Random
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import I18n.Hermeneus.ArbitraryInstances


prop_word_parser_section :: LocalizedWord -> Bool
prop_word_parser_section e = result == Right e'
  where
    e' = normalize e
    result = parse parseLocalizedWord "parse expression" (printLocalizedWord e' :: Text)
