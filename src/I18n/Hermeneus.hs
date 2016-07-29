{-# OPTIONS_GHC -F -pgmF htfpp #-}

module I18n.Hermeneus where

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

test_matchFeatureConstraint :: IO ()
test_matchFeatureConstraint = do
  assertEqual True $ matchFeatureConstraint (FeatureCondition []) $ M.fromList []
  assertEqual False $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList []
  assertEqual True $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList [("n", "s")]
  assertEqual False $ matchFeatureConstraint (FeatureCondition [("n", "s")]) $ M.fromList [("n", "d")]

matchFeatureConstraint :: FeatureCondition -> FeatureEnv -> Bool
matchFeatureConstraint = undefined

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

derefFeatures :: [FeatureEnv] -> Map String FeatureEnv -> WordReference -> Either String (Map FeatureId FeatureValue)
derefFeatures argEnvs storedWordEnvs (PlaceholderNumber phid) = pure $ argEnvs !! fromInteger phid -- ToDo: Fix about corner cases
derefFeatures argEnvs storedWordEnvs (WordKey wk) = maybeToEither ("There are no word: " ++ wk) $ M.lookup wk storedWordEnvs

resolveFeatureConstraintExpr :: [FeatureEnv] -> Map String FeatureEnv -> FeatureConstraintExpr -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExpr argEnvs storedWordEnvs (FeatureConstraintExpr fid fre) = derefFeature fre
  where
    derefFeature (ConcordWord wref) = do
      features <- derefFeatures argEnvs storedWordEnvs wref
      let v = M.lookup fid features
      return $ case v of
        Nothing -> M.empty
        Just x -> M.singleton fid x
    derefFeature (Feature feature) = pure $ M.singleton fid feature

resolveFeatureConstraintExprs :: [FeatureEnv] -> Map String FeatureEnv -> FeatureEnv -> [FeatureConstraintExpr] -> Either String (Map FeatureId FeatureValue)
resolveFeatureConstraintExprs argEnvs storedWordEnvs wordEnv fces = do
  envs <- mapM (resolveFeatureConstraintExpr argEnvs storedWordEnvs) fces
  return $ M.unions (wordEnv : envs)

resolveFeaturePlaceholder :: [FeatureEnv] -> Map String FeatureEnv -> Placeholder -> Either String (Map FeatureId FeatureValue)
resolveFeaturePlaceholder argEnvs storedWordEnvs (wref, fces) = do
  features <- derefFeatures argEnvs storedWordEnvs wref
  resolveFeatureConstraintExprs argEnvs storedWordEnvs features fces

maybeToEither :: s -> Maybe a -> Either s a
maybeToEither = flip maybe Right . Left

translateWords :: [Placeholder] -> Map String LocalizedWord -> [String]
translateWords = undefined
  where
    featureEnv (LocalizedWord fe _) = fe
    wordTranslations (LocalizedWord _ wts) = wts
