module Cauterize.Schema.Parser (parseSchema) where

import Control.Monad

import Text.Parsec
import Text.Parsec.String

import Cauterize.Schema.AST
import Cauterize.Schema.Utils
import Cauterize.Common.BuiltIn

parseSchema :: Parser Schema
parseSchema = pSexp "schema" $ do
    qname <- spacedQuoted
    qver <- spacedQuoted
    forms <- pForms
    return $ Schema qname qver (bis ++ forms)
  where
    pForms = option [] $ spaces1 >> parseForm `sepBy` spaces1 
    bis = map (FType . TBuiltIn) [minBound .. maxBound]

parseForm :: Parser SchemaForm
parseForm = liftM FType parseType

parseType :: Parser Type
parseType = choice $ map try
  [ parseScalar
  , parseConst
  , parseFixedArray
  , parseBoundedArray
  , parseStruct
  , parseEnum
  , parseSet
  , parsePad
  , parsePartial
  ]

parseScalar :: Parser Type
parseScalar = pSexp "scalar" $ liftM2 TScalar spacedName spacedBuiltIn

parseConst :: Parser Type
parseConst = pSexp "const" $ liftM3 TConst spacedName spacedBuiltIn spacedNumber

parseFixedArray :: Parser Type
parseFixedArray = pSexp "fixed" $ liftM3 TFixedArray spacedName spacedName spacedNumber

parseBoundedArray :: Parser Type
parseBoundedArray = pSexp "bounded" $ liftM3 TBoundedArray spacedName spacedName spacedNumber

parseStruct :: Parser Type
parseStruct = pSexp "struct" $ liftM2 TStruct spacedName parseFields
  where
    parseFields = many $ spaces1 >> parseField
    parseField = pSexp "field" $ liftM2 StructField spacedName spacedName

parseEnum :: Parser Type
parseEnum = pSexp "enum" $ liftM2 TEnum spacedName parseVariants
  where
    parseVariants = many $ spaces1 >> parseVariant
    parseVariant = pSexp "var" $ liftM2 EnumVariant spacedName (optionMaybe spacedName)

parseSet :: Parser Type
parseSet = pSexp "set" $ liftM2 TSet spacedName parseMembers
  where
    parseMembers = many $ spaces1 >> parseMember
    parseMember = pSexp "mem" $ liftM2 SetField spacedName spacedName

parsePad :: Parser Type
parsePad = pSexp "pad" $ liftM2 TPad spacedName spacedNumber

parsePartial :: Parser Type
parsePartial = pSexp "partial" $ liftM3 TPartial spacedName spacedNumber parseVariants
  where
    parseVariants = many $ spaces1 >> parseVariant
    parseVariant = pSexp "var" $ liftM2 PartialVariant spacedName spacedName


parseBuiltInName :: Parser BuiltIn
parseBuiltInName = liftM read $ choice names
  where
    names = map (try . string . show) bis
    bis = [minBound .. maxBound] :: [BuiltIn]

spacedBuiltIn :: Parser BuiltIn
spacedBuiltIn = spaces1 >> parseBuiltInName
