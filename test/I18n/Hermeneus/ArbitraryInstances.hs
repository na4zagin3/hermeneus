module I18n.Hermeneus.ArbitraryInstances where

import qualified Data.List.NonEmpty as NEL

import Generic.Random
import Data.Maybe
import qualified Data.Map as M
import Data.Text (Text)
import Test.Tasty.QuickCheck
import I18n.Hermeneus.Prim
import qualified I18n.Hermeneus.NumberHandling as NH

import I18n.Hermeneus.AbaList (AbaList)
import qualified I18n.Hermeneus.AbaList as ABA

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
                    , WordRef . getUnicodeString <$> arbitrary
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
  arbitrary = oneof [ TranslatedString . nonEmptyStringFromString <$> genNonEmptyAlphaNumString
                    , Placeholder <$> arbitrary
                    ]

instance (Arbitrary TranslationTemplate) where
  arbitrary = do
    abas <- arbitrary :: Gen (AbaList (Maybe NonEmptyString) Placeholder)
    let convTranslatedString = fmap TranslatedString . (>>= maybeNonEmptyStringFromString)
    let abas' = ABA.toList $ ABA.abMap convTranslatedString (Just . Placeholder) abas :: [Maybe TranslationHank]
    return . TranslationTemplate $ catMaybes abas'
  shrink = genericShrink

instance (Arbitrary NumberFeature) where
  arbitrary = do
    e <- NH.normalize <$> arbitrary
    i <- genNonEmptyAlphaNumString
    return $ NumberFeature e i
  shrink = genericShrink

instance (Arbitrary NumberHandling) where
  arbitrary = NumberHandling <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance (Arbitrary LangInfo) where
  arbitrary = genericArbitrary uniform
  shrink = genericShrink

instance (Arbitrary FeatureCondition) where
  arbitrary = do
    n <- getSize
    k <- choose (0,n)
    fs <- vectorOf n genNonEmptyAlphaNumString
    vs <- vectorOf n genNonEmptyAlphaNumString
    return . FeatureCondition . M.fromList $ zip fs vs
  shrink = genericShrink

instance (Arbitrary LocalizedWord) where
  arbitrary = do
    n <- getSize
    k <- choose (0,n)
    fs <- vectorOf n genNonEmptyAlphaNumString
    vs <- vectorOf n genNonEmptyAlphaNumString
    let env = M.fromList $ zip fs vs
    LocalizedWord env <$> scale (`div` 2) arbitrary
  shrink = genericShrink

instance (Arbitrary SentenceKey) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink

instance (Arbitrary WordKey) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink

instance (Arbitrary TranslationSet) where
  arbitrary = do
    n <- getSize
    k <- choose (0,n)
    fs <- arbitrary
    m1 <- M.fromList <$> vectorOf n (scale (`div` 3) arbitrary)
    m2 <- M.fromList <$> vectorOf n (scale (`div` 3) arbitrary)
    return $ TranslationSet fs m1 m2
  shrink = genericShrink
