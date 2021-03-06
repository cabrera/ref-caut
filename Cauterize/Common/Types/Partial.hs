{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.Types.Partial where

import Cauterize.Common.Primitives
import Cauterize.Common.IndexedRef
import Cauterize.Common.References

import Data.List

data TPartial t = TPartial Name [IndexedRef t]
  deriving (Show, Ord, Eq)

instance References (TPartial Name) where
  referencesOf (TPartial _ rs) = nub $ map refRef rs
