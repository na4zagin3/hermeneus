module I18n.Hermeneus.NumberHandlingSpec where

import Data.Text
import Text.Parsec

import Generic.Random
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import I18n.Hermeneus.NumberHandling

instance (Arbitrary Expr) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink


prop_parser_section :: Expr -> Bool
prop_parser_section e = (fmap normalize $ parse expr "parse expression" (printExpr e :: Text)) == Right (normalize e)
