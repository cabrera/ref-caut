module Cauterize.Schema.Type where

import Cauterize.Common.BuiltIn
import Data.Maybe
import qualified Cauterize.Schema.AST as AST
import qualified Data.Map as M

data Schema = Schema
  { schemaName :: String
  , schemaVersion :: String
  , schemaForms :: [SchemaForm]
  }
  deriving (Show)

data SchemaForm = FType Type
  deriving (Show)

data Type = TBuiltIn BuiltIn
          | TScalar String BuiltIn
          | TConst String BuiltIn Integer

          | TFixedArray String Type Integer
          | TBoundedArray String Type Integer

          | TStruct String [StructField]
          | TSet String [SetField]

          | TEnum String [EnumVariant]
          | TPartial String Integer [PartialVariant]

          | TPad String Integer
  deriving (Show)

data StructField = StructField String Type
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe Type)
  deriving (Show)

data PartialVariant = PartialVariant String Type
  deriving (Show)

data SetField = SetField String Type
  deriving (Show)

fromAST :: AST.Schema -> Schema
fromAST s@(AST.Schema n v _) = Schema n v fs
  where
    fs = map (FType . snd) $ M.toList (fromASTToType m)
    m = AST.schemaTypeMap s

fromASTToType :: M.Map AST.Name AST.Type -> M.Map AST.Name Type
fromASTToType m = r
  where
    r = fmap mkType m

    lookupR k = fromJust $ M.lookup k r

    mkType (AST.TBuiltIn b) = TBuiltIn b
    mkType (AST.TScalar n b) = TScalar n b
    mkType (AST.TConst n b v) = TConst n b v
    mkType (AST.TFixedArray n t s) = TFixedArray n (lookupR t) s
    mkType (AST.TBoundedArray n t s) = TBoundedArray n (lookupR t) s
    mkType (AST.TStruct n fs) = TStruct n (fromASTStructFields lookupR fs)
    mkType (AST.TSet n fs) = TSet n (fromASTSetFields lookupR fs)
    mkType (AST.TEnum n vs) = TEnum n (fromASTEnumVars lookupR vs)
    mkType (AST.TPartial n l vs) = TPartial n l (fromASTPartialVars lookupR vs)
    mkType (AST.TPad n s) = TPad n s

fromASTStructFields :: (AST.Name -> Type) -> [AST.StructField] -> [StructField]
fromASTStructFields lkUp = map go
  where
    go (AST.StructField s t) = StructField s (lkUp t)

fromASTSetFields :: (AST.Name -> Type) -> [AST.SetField] -> [SetField]
fromASTSetFields lkUp = map go
  where
    go (AST.SetField s t) = SetField s (lkUp t)

fromASTEnumVars :: (AST.Name -> Type) -> [AST.EnumVariant] -> [EnumVariant]
fromASTEnumVars lkUp = map go
  where
    go (AST.EnumVariant n Nothing) = EnumVariant n Nothing
    go (AST.EnumVariant n (Just t)) = EnumVariant n (Just $ lkUp t)

fromASTPartialVars :: (AST.Name -> Type) -> [AST.PartialVariant] -> [PartialVariant]
fromASTPartialVars lkUp = map go
  where
    go (AST.PartialVariant s t) = PartialVariant s (lkUp t)
