{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import           Test.Framework

data MessageArgument = RawString String
                 | Localized LocalizedWord
  deriving (Eq, Ord, Show, Read)

trw :: String -> LocalizedWord
trw = undefined

trs :: String -> [MessageArgument] -> LocalizedWord
trs = undefined

type FeatureId = String
type FeatureValue = String

type FeaturePair = (FeatureId, FeatureValue)

type FeatureEnv = Map FeatureId FeatureValue
data FeatureCondition = FeatureCondition [FeaturePair]
  deriving (Eq, Ord, Show, Read)

data TranslationTemplate = TranslatedString String
                         | Placeholder Placeholder
  deriving (Eq, Ord, Show, Read)

type Placeholder = (WordReference, [FeatureConstraintExpr])

data WordReference = PlaceholderNumber Integer
                   | WordKey String
  deriving (Eq, Ord, Show, Read)

data FeatureConstraintExpr = FeatureConstraintExpr FeatureId FeatureReferenceExpr
  deriving (Eq, Ord, Show, Read)

data FeatureReferenceExpr = ConcordWord WordReference
                          | Feature FeatureId
  deriving (Eq, Ord, Show, Read)

data LocalizedWord = LocalizedWord FeatureEnv [(FeatureCondition, String)]
  deriving (Eq, Ord, Show, Read)

test_selectWord1, test_selectWord2, test_selectWord3 :: IO ()
test_selectWord1 = assertEqual "a" $ selectWord (M.fromList []) [(FeatureCondition [], "a")]
test_selectWord2 = assertEqual "a" $ selectWord (M.fromList [("n", "s")]) [(FeatureCondition [("n", "s")], "a"), (FeatureCondition [], "b")]
test_selectWord3 = assertEqual "b" $ selectWord (M.fromList [("n", "d")]) [(FeatureCondition [("n", "s")], "a"), (FeatureCondition [], "b")]

-- | Select word using features.
-- If there are no candidates, the last candidate may be used.
selectWord :: FeatureEnv -> [(FeatureCondition, String)] -> String
selectWord _  [] = undefined
selectWord fe ((fc, w) : xs) | matchFeatureConstraint fc fe = w
                             | otherwise = selectWord fe xs

selectDefaultWord :: [(FeatureCondition, String)] -> String
selectDefaultWord [] = undefined
selectDefaultWord ((_, w) : _) = w

test_matchFeatureConstraint :: IO ()
test_matchFeatureConstraint = do
  assertEqual True $ matchFeatureConstraint (FeatureCondition []) $ M.fromList []
  assertEqual False $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList []
  assertEqual True $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList [("n", "s")]
  assertEqual False $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList [("n", "d")]

matchFeatureConstraint :: FeatureCondition -> FeatureEnv -> Bool
matchFeatureConstraint (FeatureCondition kvs) env = kvsMap `M.isSubmapOf` env
  where
    kvsMap = M.fromList kvs

wordFeatures :: LocalizedWord -> Set FeatureId
wordFeatures (LocalizedWord fe fcss) = M.keysSet fe `S.union` S.unions (map (extractFeatures . fst) fcss)
  where
    extractFeatures (FeatureCondition fcs) = S.fromList $ map fst fcs

  {-
test_resolveFeatures :: IO ()
test_resolveFeatures = do
  assertEqual [("b", "c")] $ resolveFeatureConstraintExpr (WordKey "a") $ M.fromList [("a", M.fromList [("b", "c")])] 
  assertEqual [("b", "c")] $ resolveFeatureConstraintExpr (WordKey "a") $ M.fromList [("a", M.fromList [("b", "c"), ("d", "e")])] 
-}

-- |
-- Positinally lefter constaint expression has priority.

derefWordReference :: [a] -> Map String a -> WordReference -> Either String a
derefWordReference argEnvs _              (PlaceholderNumber phid) = pure $ argEnvs !! fromInteger phid -- ToDo: Fix about corner cases
derefWordReference _       storedWordEnvs (WordKey wk) = maybeToEither ("There are no word: " ++ wk) $ M.lookup wk storedWordEnvs

resolveFeatureConstraintExpr :: [FeatureEnv] -> Map String FeatureEnv -> FeatureConstraintExpr -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExpr argEnvs storedWordEnvs (FeatureConstraintExpr fid fre) = derefFeature fre
  where
    derefFeature (ConcordWord wref) = do
      features <- derefWordReference argEnvs storedWordEnvs wref
      let v = M.lookup fid features
      return $ case v of
        Nothing -> M.empty
        Just x -> M.singleton fid x
    derefFeature (Feature feature) = pure $ M.singleton fid feature

resolveFeatureConstraintExprs :: [FeatureEnv] -> Map String FeatureEnv -> FeatureEnv -> [FeatureConstraintExpr] -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExprs argEnvs storedWordEnvs wordEnv fces = do
  envs <- mapM (resolveFeatureConstraintExpr argEnvs storedWordEnvs) fces
  return $ M.unions (wordEnv : envs)

resolveFeaturePlaceholder :: [FeatureEnv] -> Map String FeatureEnv -> Placeholder -> Either String FeatureEnv
resolveFeaturePlaceholder argEnvs storedWordEnvs (wref, fces) = do
  features <- derefWordReference argEnvs storedWordEnvs wref
  resolveFeatureConstraintExprs argEnvs storedWordEnvs features fces

maybeToEither :: s -> Maybe a -> Either s a
maybeToEither = flip maybe Right . Left

test_translatePlaceholders :: IO ()
test_translatePlaceholders = do
  assertEqual (Right []) $ translatePlaceholders [] M.empty []
  assertEqual (Right ["a"]) $ translatePlaceholders [(PlaceholderNumber 0, [])] M.empty [LocalizedWord M.empty [(FeatureCondition [], "a")]]
  let placeholders = [(PlaceholderNumber 1, [FeatureConstraintExpr "number" (ConcordWord (PlaceholderNumber 0))]), (PlaceholderNumber 0, [])]
  let word1 = LocalizedWord (M.fromList [("number", "single")]) [(FeatureCondition [], "one")]
  let wordCar = LocalizedWord (M.fromList []) [ (FeatureCondition [("number", "single")], "car")
                                                 , (FeatureCondition [("number", "plural")], "cars")
                                                 ]
  let argument1 = [ word1, wordCar ]
  assertEqual (Right ["one", "car"]) $ translatePlaceholders placeholders M.empty argument1

translatePlaceholders :: [Placeholder] -> Map String LocalizedWord -> [LocalizedWord] -> Either String [String]
translatePlaceholders phs storedWords args = do
    let featureEnv (LocalizedWord fe _) = fe
    let wordTranslation (LocalizedWord _ wts) = wts
    let argEnvs = map featureEnv args
    let storedWordEnvs = M.map featureEnv storedWords
    let wordTranslations = map wordTranslation args
    featureEnvs <- mapM (resolveFeaturePlaceholder argEnvs storedWordEnvs) phs
    return $ zipWith selectWord featureEnvs wordTranslations

translateTemplates :: [TranslationTemplate] -> Map String LocalizedWord -> [LocalizedWord] -> Either String [String]
translateTemplates tts storedWords args = do
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
    f :: [FeatureEnv] -> [TranslationTemplate] -> Either String [String]
    f _ [] = pure $ []
    f envs (TranslatedString str : ts) = (str :) <$> f envs ts
    f (env : envs) (Placeholder ph@(wref, _) : ts) = do
      w <- derefWordReference wordTranslations storedWordTranslations wref
      tstr <- translateTemplate env ph w
      (tstr :) <$> f envs ts
