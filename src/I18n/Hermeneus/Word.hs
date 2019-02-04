{-# LANGUAGE FlexibleContexts, OverloadedStrings, PartialTypeSignatures #-}
module I18n.Hermeneus.Word where

import Control.Applicative ((<$>))
import Data.Functor
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NEL
import Data.Maybe
import Data.Semigroup (Semigroup, (<>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.String

import Text.Parsec

import I18n.Hermeneus.Message( parseFeatureId, parseFeatureValue )
import I18n.Hermeneus.Prim

normalize :: LocalizedWord -> LocalizedWord
normalize (LocalizedWord e cw) = LocalizedWord (normalizeFeatureEnv e) (normalizeConditionalWord cw)

normalizeFeatureEnv :: FeatureEnv -> FeatureEnv
normalizeFeatureEnv (FeatureEnv fe) = FeatureEnv $ M.filterWithKey f fe
  where
    f "" _ = False
    f _ "" = False
    f _ _ = True

normalizeConditionalWord :: ConditionalWord -> ConditionalWord
normalizeConditionalWord cw = NEL.map normalizeTranslation cw

normalizeTranslation :: (FeatureCondition, String) -> (FeatureCondition, String)
normalizeTranslation (FeatureCondition cs, w) = (FeatureCondition cs', w)
  where
    cs' = M.filterWithKey f cs
    f "" _ = False
    f _ "" = False
    f _ _ = True

parseConditionalWord :: Stream s m Char => ParsecT s u m ConditionalWord
parseConditionalWord = do
  NEL.fromList <$> parseFeature `sepBy1` (string ";")

printConditionalWord :: (IsString s, Semigroup s, Monoid s) => ConditionalWord -> s
printConditionalWord fs = mconcat (intersperse (fromString ";") ts)
  where
    ts = map printFeature $ NEL.toList fs

parseLocalizedWord :: Stream s m Char => ParsecT s u m LocalizedWord
parseLocalizedWord = do
  featureEnv <- parseFeatureEnv
  string "~"
  cw <- parseConditionalWord
  return $ LocalizedWord featureEnv cw

printLocalizedWord :: (IsString s, Semigroup s, Monoid s) => LocalizedWord -> s
printLocalizedWord (LocalizedWord e cw) = printFeatureEnv e <> "~" <> printConditionalWord cw

parseFeatureEnv :: Stream s m Char => ParsecT s u m FeatureEnv
parseFeatureEnv = do
  entries <- parseFeatureEnvEntry `sepBy` (string ",")
  return . FeatureEnv $ M.fromList entries

parseFeatureEnvEntry :: Stream s m Char => ParsecT s u m (FeatureId, FeatureValue)
parseFeatureEnvEntry = do
  i <- parseFeatureId
  string "="
  v <- parseFeatureValue
  return $ (i, v)

printFeatureEnv :: (IsString s, Semigroup s, Monoid s) => FeatureEnv -> s
printFeatureEnv (FeatureEnv fe ) = mconcat . intersperse (fromString ",") . map printEntry . M.toList $ fe
  where
    printEntry (i, v) = mconcat [ fromString i, fromString "=", fromString v]

parseFeature :: Stream s m Char => ParsecT s u m (FeatureCondition, String)
parseFeature = do
  cond <- parseFeatureCondition
  string ":"
  string "\""
  w <- (many $ (try parseEscaped <|> noneOf "\""))
  string "\""
  return (cond, w)
  where
    parseEscaped = do
      string "\\"
      (string "\\" $> '\\') <|> (string "\"" $> '"')

printFeature :: (IsString s, Semigroup s, Monoid s) => (FeatureCondition, String) -> s
printFeature (cond, w) = mconcat
    [ printFeatureCondition cond
    , fromString ":\""
    , fromString (concatMap escape w)
    , fromString "\""
    ]
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape x = [x]

parseFeatureCondition :: Stream s m Char => ParsecT s u m FeatureCondition
parseFeatureCondition = do
  entries <- parseFeatureEnvEntry `sepBy` (string ",")
  return . FeatureCondition $ M.fromList entries

printFeatureCondition :: (IsString s, Semigroup s, Monoid s) => FeatureCondition -> s
printFeatureCondition (FeatureCondition m) = mconcat . intersperse (fromString ",") . map printEntry $ M.toList m
  where
    printEntry (i, v) = mconcat [ fromString i, fromString "=", fromString v]
