module Main where

import Cauterize.Options
import Cauterize.Schema
import qualified Cauterize.Schema.Type as SCHE
import qualified Cauterize.Specification as SPEC

import Text.PrettyPrint.Class

main :: IO ()
main = runWithOptions $ \opts -> parseFile (inputFile opts) >>= render
  where
    render :: Either String SCHE.Schema -> IO ()
    render result = case result of
                      (Left e) -> print e
                      (Right r) -> do
                        print r
                        case SPEC.fromSchema r of
                          Just spec -> print $ pretty spec
                          Nothing -> error "ERROR: Inconsistent schema."
