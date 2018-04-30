{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module I18n.Hermeneus.Message where

import Control.Applicative ((<$>))
import Data.Functor
import Data.List (intersperse)
import Data.Maybe
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.String

import Text.Parsec

import I18n.Hermeneus.Prim

-- ToDo: Provide escape sequence in word reference

parseTranslationTemplate :: Stream s m Char => ParsecT s u m TranslationTemplate
parseTranslationTemplate = TranslationTemplate <$> many parseTranslationHank

parseTranslationHank :: Stream s m Char => ParsecT s u m TranslationHank
parseTranslationHank = parseTranslatedString <|> parsePlaceholder

parsePlaceholder :: Stream s m Char => ParsecT s u m TranslationHank
parsePlaceholder = do
  string "{"
  wRef <- parseWordReference
  exprs <- option [] $ string ":" *> many (parseFeatureConstraintExpr <* option [] (string ","))
  string "}"
  return $ Placeholder (wRef, exprs)

parseTranslatedString :: Stream s m Char => ParsecT s u m TranslationHank
parseTranslatedString = TranslatedString . nonEmptyStringFromString <$> many1 parseTranslatedStringChar

parseTranslatedStringChar :: Stream s m Char => ParsecT s u m Char
parseTranslatedStringChar = try escapedBrace <|> noneOf "{}"
  where
    escapedBrace :: Stream s m Char => ParsecT s u m Char
    escapedBrace = (string "{{" $> '{') <|> (string "}}" $> '}')

parseWordReference :: Stream s m Char => ParsecT s u m WordReference
parseWordReference = parseWord <|> parseNumber
  where
    parseNumber = PlaceholderNumber . read <$> many1 digit
    parseWord = do
      string "*"
      (fmap WordKey . many $ (try escapedStar <|> noneOf "*")) <* string "*"
    escapedStar :: Stream s m Char => ParsecT s u m Char
    escapedStar = string "**" $> '*'

parseFeatureConstraintExpr :: Stream s m Char => ParsecT s u m FeatureConstraintExpr
parseFeatureConstraintExpr = do
  i <- parseFeatureId
  expr <- parseFeatureReferenceExpr
  return $ FeatureConstraintExpr i expr

parseFeatureId :: Stream s m Char => ParsecT s u m FeatureId
parseFeatureId = featureIdFromString <$> many1 alphaNum

parseFeatureValue :: Stream s m Char => ParsecT s u m FeatureValue
parseFeatureValue = featureValueFromString <$> many1 alphaNum

parseFeatureReferenceExpr :: Stream s m Char => ParsecT s u m FeatureReferenceExpr
parseFeatureReferenceExpr = parseConcordWord <|> parseFeature
  where
    parseConcordWord = do
      string "#"
      wordRef <- parseWordReference
      return $ ConcordWord wordRef
    parseFeature = do
      string "="
      value <- parseFeatureValue
      return $ Feature value


printTranslationTemplate :: (IsString s, Semigroup s, Monoid s) => TranslationTemplate -> s
printTranslationTemplate (TranslationTemplate hs) = mconcat $ map printTranslationHank hs

printTranslationHank :: (IsString s, Semigroup s, Monoid s) => TranslationHank -> s
printTranslationHank (TranslatedString s) = fromString $ concatMap escapeBrace s
  where
    escapeBrace x | x == '{' || x == '}' = [x, x]
    escapeBrace x = [x]
printTranslationHank (Placeholder p) = printPlaceholder p

printPlaceholder :: (IsString s, Semigroup s, Monoid s) => Placeholder -> s
printPlaceholder (wr, fces) = mconcat [ "{"
                                      , printWordReference wr
                                      , if null fces
                                        then mempty
                                        else ":" <> fstr
                                      , "}"
                                      ]
  where
    fstr = mconcat $ intersperse "," $ map printFeatureConstraintExpr fces

printWordReference ::  (IsString s, Semigroup s, Monoid s) => WordReference -> s
printWordReference (PlaceholderNumber n) = fromString $ show n
printWordReference (WordKey w) = "*" <> fromString (concatMap escapeStar w) <> "*"
  where
    escapeStar '*' = "**"
    escapeStar x = [x]

printFeatureConstraintExpr :: (IsString s, Semigroup s, Monoid s) => FeatureConstraintExpr -> s
printFeatureConstraintExpr (FeatureConstraintExpr i e) = printFeatureId i <> printFeatureReferenceExpr e

printFeatureReferenceExpr :: (IsString s, Semigroup s, Monoid s) => FeatureReferenceExpr -> s
printFeatureReferenceExpr (ConcordWord w) = "#" <> printWordReference w
printFeatureReferenceExpr (Feature f) = "=" <> printFeatureValue f

printFeatureId :: (IsString s, Semigroup s, Monoid s) => FeatureId -> s
printFeatureId = fromString . nonEmptyStringToString

printFeatureValue :: (IsString s, Semigroup s, Monoid s) => FeatureValue -> s
printFeatureValue = fromString . nonEmptyStringToString
