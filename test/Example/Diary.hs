module Example.Diary where

import Control.Exception
import Control.Monad
import qualified Data.Map as M
import qualified Data.Yaml as Y
import I18n.Hermeneus.Aeson ()
import I18n.Hermeneus.Engine (translateMessage)
import I18n.Hermeneus.Prim
import System.FilePath
import System.FilePath.Glob

import Test.Tasty.HUnit

resourceDir :: FilePath
resourceDir = "test-resource/Example/Diary"

translationResource :: IO Database
translationResource = do
  files <- globDir1 (compile "*.yaml") resourceDir
  translationSets <- forM files $ \fp -> do
    let locale = takeFileName $ dropExtensions fp
    s <- either throw id <$> Y.decodeFileEither fp
    return (locale, s)
  return $ M.fromList translationSets

unit_translationResource :: IO ()
unit_translationResource = do
  db <- translationResource
  M.null db @?= False

wordRome, wordAthens :: MessageArg
sentenceTrip :: MessageKey
wordRome   = ArgWord "Rome"   "" "com.example.diary.trip"
wordAthens = ArgWord "Athens" "" "com.example.diary.trip"
sentenceTrip = MessageKey "Went to {0} from {1}." "com.example.diary.trip"

unit_translations :: IO ()
unit_translations = do
  db <- translationResource
  translateMessage db "en"  sentenceTrip [wordRome, wordAthens] @?= Right "Went to Rome from Athens."
  translateMessage db "en"  sentenceTrip [wordAthens, wordRome] @?= Right "Went to Athens from Rome."
  translateMessage db "ja"  sentenceTrip [wordRome, wordAthens] @?= Right "アテネからローマへ行った。"
  translateMessage db "ja"  sentenceTrip [wordAthens, wordRome] @?= Right "ローマからアテネへ行った。"
  translateMessage db "grc" sentenceTrip [wordRome, wordAthens] @?= Right "εἰς Ῥώμην ἦλθον ἐξ Ἀθήνων."
  translateMessage db "grc" sentenceTrip [wordAthens, wordRome] @?= Right "εἰς Ἀθήνας ἦλθον ἐξ Ῥώμης."

