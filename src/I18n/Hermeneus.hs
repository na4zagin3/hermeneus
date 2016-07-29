module I18n.Hermeneus where

data LocalizedWord = LocalizedWord String
  deriving (Eq, Ord, Show, Read)

data Placeholder = RawString String
                 | Localized LocalizedWord
  deriving (Eq, Ord, Show, Read)

trw :: String -> LocalizedWord
trw = undefined

trs :: String -> [Placeholder] -> LocalizedWord
trs = undefined
