module I18n.Hermeneus.ArbitraryInstances where

import Data.Text

import qualified Data.List.NonEmpty as NEL

import Generic.Random
import Test.Tasty.QuickCheck
import I18n.Hermeneus.Prim
import qualified I18n.Hermeneus.NumberHandling as NH

instance (Arbitrary NH.Expr) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink

genAlphaNumChar :: Gen Char
genAlphaNumChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genNonEmptyAlphaNumString :: Gen String
genNonEmptyAlphaNumString = listOf1 genAlphaNumChar

instance (Arbitrary a) => Arbitrary (NEL.NonEmpty a) where
  arbitrary = NEL.fromList <$> listOf1 arbitrary
  shrink = genericShrink

instance Arbitrary WordReference where
  arbitrary = oneof [ PlaceholderNumber . getNonNegative <$> arbitrary
                    , WordKey . getUnicodeString <$> arbitrary
                    ]
  shrink = genericShrink

instance (Arbitrary FeatureConstraintExpr) where
  arbitrary = do
    f <- nonEmptyStringFromString <$> genNonEmptyAlphaNumString
    expr <- arbitrary
    return $ FeatureConstraintExpr f expr
  shrink = genericShrink

instance (Arbitrary FeatureReferenceExpr) where
  arbitrary = oneof [ ConcordWord <$> arbitrary
                    , Feature . nonEmptyStringFromString <$> genNonEmptyAlphaNumString
                    ]

instance (Arbitrary TranslationHank) where
  arbitrary = oneof [ TranslatedString <$> return (nonEmptyStringFromString "a")
                    , Placeholder <$> arbitrary
                    ]

instance (Arbitrary TranslationTemplate) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink

