{-# LANGUAGE FlexibleInstances #-}
module Cauterize.Common.IndexedRef where

import Cauterize.Common.Primitives

import qualified Data.Map as M
import Data.Maybe as M

data IndexedRef r = IndexedRef Name r Integer
  deriving (Show, Ord, Eq)

refSig :: M.Map Name Signature -> IndexedRef Name -> Signature
refSig sm (IndexedRef n m _) = concat ["(field ", n, " ", luSig m, ")"]
  where
    luSig na = fromJust $ na `M.lookup` sm

refRef :: IndexedRef t -> t
refRef (IndexedRef _ n _) = n
