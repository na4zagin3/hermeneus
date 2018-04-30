{-# LANGUAGE FlexibleContexts #-}

module I18n.Hermeneus.Message where

import Control.Applicative ((<$>))
import Data.Functor
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
  exprs <- option [] $ string ":" *> many parseFeatureConstraintExpr
  string "}"
  return $ Placeholder (wRef, exprs)

parseTranslatedString :: Stream s m Char => ParsecT s u m TranslationHank
parseTranslatedString = TranslatedString <$> many1 parseTranslatedStringChar

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
      fmap WordKey . many $ noneOf ":"

parseFeatureConstraintExpr :: Stream s m Char => ParsecT s u m FeatureConstraintExpr
parseFeatureConstraintExpr = do
  i <- parseFeatureId
  expr <- parseFeatureReferenceExpr
  return $ FeatureConstraintExpr i expr

parseFeatureId :: Stream s m Char => ParsecT s u m FeatureId
parseFeatureId = many1 alphaNum

parseFeatureValue :: Stream s m Char => ParsecT s u m FeatureValue
parseFeatureValue = many1 alphaNum

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
