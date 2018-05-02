module I18n.Hermeneus.Engine where

import Control.Applicative
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import I18n.Hermeneus.Prim
import I18n.Hermeneus.Database
import I18n.Hermeneus.Message

import Text.Parsec (parse)

-- type StoredWordMap a = Map (String, String, Context)
type StoredWordMap a = Map String a


-- | Select word using features.
-- If there are no candidates, the last candidate may be used.
selectWord :: FeatureEnv -> [(FeatureCondition, String)] -> String
selectWord _  [] = error "selectWord"
selectWord _  [(_, w)] = w
selectWord fe ((fc, w) : xs) | matchFeatureConstraint fc fe = w
                             | otherwise = selectWord fe xs

selectDefaultWord :: [(FeatureCondition, String)] -> String
selectDefaultWord [] = error "selectDefaultWord"
selectDefaultWord ((_, w) : _) = w

matchFeatureConstraint :: FeatureCondition -> FeatureEnv -> Bool
matchFeatureConstraint (FeatureCondition kvs) env = kvsMap `M.isSubmapOf` env
  where
    kvsMap = M.fromList kvs

wordFeatures :: LocalizedWord -> Set FeatureId
wordFeatures (LocalizedWord fe fcss) = M.keysSet fe `S.union` S.unions (map (extractFeatures . fst) fcss)
  where
    extractFeatures (FeatureCondition fcs) = S.fromList $ map fst fcs


-- |
-- Positinally lefter constaint expression has priority.

derefWordReference :: [a] -> StoredWordMap a -> WordReference -> Either String a
derefWordReference argEnvs _              (PlaceholderNumber phid) = pure $ argEnvs !! fromInteger phid -- ToDo: Fix about corner cases
derefWordReference _       storedWordEnvs (WordRef wk) = maybeToEither ("There are no word: " ++ wk) $ M.lookup wk storedWordEnvs

resolveFeatureConstraintExpr :: [FeatureEnv] -> StoredWordMap FeatureEnv -> FeatureConstraintExpr -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExpr argEnvs storedWordEnvs (FeatureConstraintExpr fid fre) = derefFeature fre
  where
    derefFeature (ConcordWord wref) = do
      features <- derefWordReference argEnvs storedWordEnvs wref
      let v = M.lookup fid features
      return $ case v of
        Nothing -> M.empty
        Just x -> M.singleton fid x
    derefFeature (Feature feature) = pure $ M.singleton fid feature

resolveFeatureConstraintExprs :: [FeatureEnv] -> StoredWordMap FeatureEnv -> FeatureEnv -> [FeatureConstraintExpr] -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExprs argEnvs storedWordEnvs wordEnv fces = do
  envs <- mapM (resolveFeatureConstraintExpr argEnvs storedWordEnvs) fces
  return $ M.unions (wordEnv : envs)

resolveFeaturePlaceholder :: [FeatureEnv] -> StoredWordMap FeatureEnv -> Placeholder -> Either String FeatureEnv
resolveFeaturePlaceholder argEnvs storedWordEnvs (wref, fces) = do
  features <- derefWordReference argEnvs storedWordEnvs wref
  resolveFeatureConstraintExprs argEnvs storedWordEnvs features fces

maybeToEither :: s -> Maybe a -> Either s a
maybeToEither = flip maybe Right . Left

translatePlaceholders :: [Placeholder] -> StoredWordMap LocalizedWord -> [LocalizedWord] -> Either String [String]
translatePlaceholders phs storedWords args = do
    let featureEnv (LocalizedWord fe _) = fe
    let wordTranslation (LocalizedWord _ wts) = wts
    let argEnvs = map featureEnv args
    let storedWordEnvs = M.map featureEnv storedWords
    let wordTranslations = map wordTranslation args
    featureEnvs <- mapM (resolveFeaturePlaceholder argEnvs storedWordEnvs) phs
    return $ zipWith selectWord featureEnvs wordTranslations

translateTemplates :: TranslationTemplate -> StoredWordMap LocalizedWord -> [LocalizedWord] -> Either String [String]
translateTemplates (TranslationTemplate tts) storedWords args = do
  featureEnvs <- mapM (resolveFeaturePlaceholder argEnvs storedWordEnvs) phs
  f featureEnvs tts
  where
    featureEnv (LocalizedWord fe _) = fe
    wordTranslation (LocalizedWord _ wts) = wts
    isPlaceholder (Placeholder ph) = Just ph
    isPlaceholder (TranslatedString _) = Nothing
    phs = mapMaybe isPlaceholder tts
    argEnvs = map featureEnv args
    storedWordEnvs = M.map featureEnv storedWords
    wordTranslations = map wordTranslation args
    storedWordTranslations = M.map wordTranslation storedWords
    -- translateTemplate :: FeatureEnv -> Placeholder -> LocalizedWord -> Either String String
    translateTemplate :: FeatureEnv -> Placeholder -> [(FeatureCondition, String)] -> Either String String
    translateTemplate env _ wts = return $ selectWord env wts
    f :: [FeatureEnv] -> [TranslationHank] -> Either String [String]
    f _ [] = pure []
    f envs (TranslatedString str:ts) = (nonEmptyStringToString str :) <$> f envs ts
    f (env:envs) (Placeholder ph@(wref, _):ts) = do
      w <- derefWordReference wordTranslations storedWordTranslations wref
      tstr <- translateTemplate env ph w
      (tstr :) <$> f envs ts

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right x) = Right x

filterWord :: MessageArg -> Maybe (String, String, Context)
filterWord (ArgWord s p c) = Just (s, p, c)
filterWord               _ = Nothing

-- hasTranslation :: Database -> String -> (String, Context) -> [(String, Context)] -> Bool
translateMessage :: Database -> Locale -> MessageKey -> [MessageArg] -> Either String String
translateMessage db l (MessageKey s c) as = do
  let ws = mapMaybe filterWord as
  let isTranslatable = hasTranslation db l (s, c) ws
  let locale = if isTranslatable then l else "en"
  ts <- maybeToEither "A" $ M.lookup locale db
  let localizedWords = map (localizeArgument ts) as
  let defaultTemplate = mapLeft show $ parse parseTranslationTemplate "translateTemplate" s :: Either String TranslationTemplate
  let localizedTemplate = maybeToEither "B" $ getLocalizedTemplate ts (s, c) :: Either String TranslationTemplate
  localizedTemplate <- localizedTemplate <|> defaultTemplate
  let storedWord = M.fromList . map (\((s, p, c), x) -> (s ++ "." ++ p, x)) $ M.toList $ translationWords ts
  concat <$> translateTemplates localizedTemplate storedWord localizedWords


exampleCar = ArgWord "car" "cars" ""
exampleBook = ArgWord "book" "books" ""
examplePiller = ArgWord "pillar" "pillars" ""
exampleMsg = MessageKey "Bought {0} {1:number#0}." ""
