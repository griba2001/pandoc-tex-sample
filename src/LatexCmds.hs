{-# LANGUAGE PackageImports, DataKinds #-}

module LatexCmds ( 
  docClass,
  withNamedEnv,    
) where

import Data.Semigroup ((<>))

import "fin" Data.Type.Nat (Nat(Z, S), SNatI)
import "vec" Data.Vec.Pull (Vec)
import qualified "vec" Data.Vec.Pull as Vec

import "parsec" Text.Parsec.String (Parser)
import qualified "parsec" Text.Parsec.Prim as P

import BasicLatex (cmdWithParams, eol)

docClass :: Parser ([String], [String])
docClass = cmdWithParams "documentclass"

beginEnv :: SNatI n => String -> Vec n String -> Parser ([String], Vec (S n) String)
beginEnv env envStack = do
    
   (optParams, [param]) <- cmdWithParams "begin"
   if param /= env 
     then P.parserFail $ "begin err. expected " <> env <> " found " <> param
     else return (optParams, env `Vec.cons` envStack)

endEnv :: SNatI n => Vec (S n) String -> Parser (Vec n String)
endEnv envStack = do
    
   (_, [param]) <- cmdWithParams "end"
   if param /= Vec.head envStack
     then P.parserFail "closing err."
     else return $ Vec.tail envStack
     

withNamedEnv :: SNatI n => String -> Vec n String -> (Vec (S n) String -> [String] -> Parser a) -> Parser a     
withNamedEnv env envStack envParser = do
    (optParams, envStack') <- beginEnv env envStack
    eol
    result <- envParser envStack' optParams
    eol
    _ <- endEnv envStack'
    return result
