module Cauterize.Schema.AST
  ( Schema(..)
  , SchemaForm(..)
  , Type(..)
  , StructField(..)
  , EnumVariant(..)
  , SetField(..)
  , PartialVariant(..)
  , Name
  , TypeIdMap

  , schemaTypeMap
  , schemaTypeIdMap
  , schemaStructuralHash

  , checkSchema
  ) where

import Cauterize.Common.BuiltIn
import Cauterize.Common.Named
import Cauterize.FormHash
import Data.List
import Data.Maybe

import Data.Graph

import Text.PrettyPrint
import Text.PrettyPrint.Class

import qualified Data.Map as M

type Name = String
type Cycle = [Name]
type TypeIdMap = M.Map Name FormHash

data Schema = Schema
  { schemaName :: String
  , schemaVersion :: String
  , schemaForms :: [SchemaForm]
  }
  deriving (Show)

data SchemaForm = FType Type
  deriving (Show)

{- 
 - Valid types in Cauterize
 -
 -    * BuiltIn
 -    * Enumeration
 -    * BoundedArray
 -    * UnboundedArray
 -    * Set
 -    * Constant
 -}
data Type = TBuiltIn BuiltIn
          | TScalar String BuiltIn
          | TConst String BuiltIn Integer

          | TFixedArray String Name Integer
          | TBoundedArray String Name Integer

          | TStruct String [StructField]
          | TSet String [SetField]

          | TEnum String [EnumVariant]
          | TPartial String Integer [PartialVariant]

          | TPad String Integer
  deriving (Show)

data StructField = StructField String Name
  deriving (Show)

data EnumVariant = EnumVariant String (Maybe Name)
  deriving (Show)

data PartialVariant = PartialVariant String Name
  deriving (Show)

data SetField = SetField String Name
  deriving (Show)

-- | This function serves two purposes:
--    1. If there are cycles in the schema, they are reported.
--    2. If the schema is valid, then a Map of names to Type IDs are produced.
schemaTypeIdMap :: Schema -> Either [Cycle] TypeIdMap
schemaTypeIdMap schema = case schemaCycles schema of
                          [] -> Right resultMap
                          cs -> Left cs
  where
    tyMap = schemaTypeMap schema
    resultMap = fmap hashType tyMap

    -- YO! There's a fromJust here. The way the input map is constructed
    -- should keep us from having to worry about this.
    hashType t = let dirRefs = fromJust $ mapM (`M.lookup` resultMap) (referredNames t)
                 in finalize $ foldl formHashWith (formHashCtx t) dirRefs

schemaCycles :: Schema -> [Cycle]
schemaCycles s = typeCycles (map snd $ M.toList tyMap)
  where
    tyMap = schemaTypeMap s

schemaTypeMap :: Schema -> M.Map Name Type
schemaTypeMap (Schema _ _ fs) = M.fromList $ map (\(FType t) -> (cautName t, t)) fs

typeCycles :: [Type] -> [Cycle]
typeCycles ts = let ns = map (\t -> (cautName t, cautName t, referredNames t)) ts
                in mapMaybe isScc (stronglyConnComp ns)
  where
    isScc (CyclicSCC vs) = Just vs
    isScc _ = Nothing

schemaStructuralHash :: Schema -> FormHash
schemaStructuralHash s@(Schema n v fs) =
    let (Right m) = schemaTypeIdMap s
        ctx = hashInit `hashFn` n `hashFn` v
    in finalize $ foldl (hshFn m) ctx fs
  where
    hshFn :: TypeIdMap -> HashContext -> SchemaForm -> HashContext
    hshFn m ctx (FType t) = let tyId = fromJust $ cautName t `M.lookup` m
                            in ctx `formHashWith` tyId

referredNames :: Type -> [Name]
referredNames (TBuiltIn _) = []
referredNames (TScalar _ b) = [cautName b]
referredNames (TConst _ b _) = [cautName b]
referredNames (TFixedArray _ n _) = [n]
referredNames (TBoundedArray _ n _) = [n]
referredNames (TStruct _ fs) = nub $  map (\(StructField _ n) -> n) fs
referredNames (TSet _ fs) = nub $  map (\(SetField _ n) -> n) fs
referredNames (TEnum _ vs) = nub $ mapMaybe (\(EnumVariant _ n) -> n) vs
referredNames (TPartial _ _ fs) = nub $ map (\(PartialVariant _ n) -> n) fs
referredNames (TPad _ _) = []

data SchemaErrors = DuplicateNames [Name]
                  | Cycles [Cycle]
  deriving (Show)

checkSchema :: Schema -> [SchemaErrors]
checkSchema s@(Schema _ _ fs) = catMaybes [duplicateNames, cycles]
  where
    duplicateNames = case duplicates $ map (\(FType t) -> cautName t) fs of
                        [] -> Nothing
                        ds -> Just $ DuplicateNames ds
    cycles = case schemaCycles s of
                [] -> Nothing
                cs -> Just $ Cycles cs


duplicates :: (Eq a, Ord a) => [a] -> [a]
duplicates ins = map fst $ M.toList dups
  where
    dups = M.filter (>1) counts
    counts = foldl insertWith M.empty ins
    insertWith m x = M.insertWith ((+) :: (Int -> Int -> Int)) x 1 m

instance CautName Type where
  cautName (TBuiltIn b) = show b
  cautName (TScalar n _) = n
  cautName (TConst n _ _) = n
  cautName (TFixedArray n _ _) = n
  cautName (TBoundedArray n _ _) = n
  cautName (TStruct n _) = n
  cautName (TSet n _) = n
  cautName (TEnum n _) = n
  cautName (TPartial n _ _) = n
  cautName (TPad n _) = n

-- Note: these instances only hash on the *value* of the type. This hash does
-- not take into account the structure of depended-uppon types.
instance Hashable Type where
  formHashWith ctx (TBuiltIn b) = ctx `hashFn` "built-in" `formHashWith` b
  formHashWith ctx (TScalar n b) = ctx `hashFn` "scalar" `hashFn` n `formHashWith` b
  formHashWith ctx (TConst n b i) = ctx `hashFn` "const" `hashFn` n `formHashWith` b `hashFn` padShowInteger i
  formHashWith ctx (TFixedArray n m i) = ctx `hashFn` "fixed" `hashFn` n `hashFn` m `hashFn` padShowInteger i
  formHashWith ctx (TBoundedArray n m i) = ctx `hashFn` "bounded" `hashFn` n `hashFn` m `hashFn` padShowInteger i
  formHashWith ctx (TStruct n sfs) = ctx `hashFn` "struct" `hashFn` n `formHashWith` sfs
  formHashWith ctx (TSet n sfs) = ctx `hashFn` "set" `hashFn` n `formHashWith` sfs
  formHashWith ctx (TEnum n evs) = ctx `hashFn` "enum" `hashFn` n `formHashWith` evs
  formHashWith ctx (TPartial n i pfs) = ctx `hashFn` "partial" `hashFn` n `hashFn` padShowInteger i `formHashWith` pfs
  formHashWith ctx (TPad n i) = ctx `hashFn` "pad" `hashFn` n `hashFn` padShowInteger i

instance Hashable StructField where
  formHashWith ctx (StructField n m) = ctx `hashFn` n `hashFn` m

instance Hashable SetField where
  formHashWith ctx (SetField n m) = ctx `hashFn` n `hashFn` m

instance Hashable EnumVariant where
  formHashWith ctx (EnumVariant n (Just m)) = ctx `hashFn` n `hashFn` m
  formHashWith ctx (EnumVariant n Nothing) = ctx `hashFn` n

instance Hashable PartialVariant where
  formHashWith ctx (PartialVariant n m) = ctx `hashFn` n `hashFn` m


instance Pretty Schema where
  pretty (Schema n v fs) = parens $ text "schema" <+> (doubleQuotes . text) n <+> (doubleQuotes . text) v <+> pfs
    where
      pfs = vcat $ map pretty fs

instance Pretty SchemaForm where
  pretty (FType t) = pretty t

instance Pretty Type where
  pretty (TBuiltIn _) = empty
  pretty (TScalar n b) = parens $ text "scalar" <+> text n <+> (text . show) b
  pretty (TConst n b i) = parens $ text "const" <+> text n <+> (text . show) b <+> (text . show) i
  pretty (TFixedArray n m i) = parens $ text "fixed" <+> text n <+> text m <+> (text . show) i
  pretty (TBoundedArray n m i) = parens $ text "bounded" <+> text n <+> text m <+> (text . show) i
  pretty (TStruct n sfs) = parens $ text "struct" <+> text n <+> psfs
    where
      psfs = vcat $ map pretty sfs
  pretty (TSet n sfs) = parens $ text "set" <+> text n <+> psfs
    where
      psfs = vcat $ map pretty sfs
  pretty (TEnum n evs) = parens $ text "enum" <+> text n <+> pevs
    where
      pevs = vcat $ map pretty evs
  pretty (TPartial n i pfs) = parens $ text "partial" <+> text n <+> (text . show) i <+> ppfs
    where
      ppfs = vcat $ map pretty pfs
  pretty (TPad n i) = parens $ text "pad" <+> text n <+> (text . show) i


instance Pretty StructField where
  pretty (StructField n m) = parens $ text "field" <+> text n <+> text m

instance Pretty EnumVariant where
  pretty (EnumVariant n Nothing) = parens $ text "var" <+> text n
  pretty (EnumVariant n (Just m)) = parens $ text "var" <+> text n <+> text m

instance Pretty SetField where
  pretty (SetField n m) = parens $ text "mem" <+> text n <+> text m

instance Pretty PartialVariant where
  pretty (PartialVariant n m) = parens $ text "var" <+> text n <+> text m

padShowInteger :: Integer -> String
padShowInteger v = let w = 20
                       v' = abs v
                       v'' = show v'
                       num = replicate (w - length v'') '0' ++ v''
                   in if v < 0
                        then '-':num
                        else '+':num
