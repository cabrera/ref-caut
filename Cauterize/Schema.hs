module Cauterize.Schema 
  ( parseFile
  , parseString
  ) where

import qualified Cauterize.Schema.Parser as P
import qualified Cauterize.Schema.AST as AST
import qualified Cauterize.Schema.Type as T

import Text.Parsec

parseFile :: FilePath -> IO (Either String T.Schema)
parseFile path = readFile path >>= parseString path

parseString :: FilePath -> String -> IO (Either String T.Schema)
parseString path str = 
    return $ case parse P.parseSchema path str of
                Left e -> Left $ show e
                Right s -> check s
  where
    check s = case AST.checkSchema s of
                [] -> Right $ T.fromAST s
                es -> Left $ show es
