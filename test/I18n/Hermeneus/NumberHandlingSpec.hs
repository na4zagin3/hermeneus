module I18n.Hermeneus.NumberHandlingSpec where

import Control.Applicative ((<$>))
import Data.Text
import Text.Parsec

import I18n.Hermeneus.NumberHandling

import Generic.Random
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import I18n.Hermeneus.ArbitraryInstances


prop_parser_section :: Expr -> Bool
prop_parser_section e = (normalize <$> parse expr "parse expression" (printExpr e :: Text)) == Right (normalize e)
