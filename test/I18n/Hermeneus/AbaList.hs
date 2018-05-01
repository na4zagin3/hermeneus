{-# LANGUAGE DeriveGeneric #-}

module I18n.Hermeneus.AbaList where

import Control.Arrow
import Data.Typeable
import Generic.Random
import GHC.Generics (Generic)

import Test.Tasty.QuickCheck

data AbaList a b = Nil | AbaList a [(b, a)]
  deriving (Show, Read, Ord, Eq, Generic, Typeable)

toList :: AbaList a a -> [a]
toList Nil = []
toList (AbaList a bas) = a : concatMap (\(b, a) -> [b, a]) bas

abMap :: (a -> a') -> (b -> b') -> AbaList a b ->AbaList a' b'
abMap f g Nil = Nil
abMap f g (AbaList a bas) = AbaList (f a) $ map (g *** f) bas

instance (Arbitrary a, Arbitrary b) => Arbitrary (AbaList a b) where
  arbitrary = genericArbitrary' uniform
  shrink = genericShrink
