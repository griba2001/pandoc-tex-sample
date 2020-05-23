{-# LANGUAGE PackageImports, DataKinds #-}

module LatexCmds where

import Data.Semigroup ((<>))

import "fin" Data.Type.Nat (Nat(Z, S), SNatI)
import "vec" Data.Vec.Pull (Vec)
import qualified "vec" Data.Vec.Pull as Vec

import "parsec" Text.Parsec.String (Parser)
import qualified "parsec" Text.Parsec.Prim as P

import BasicLatex (cmdWithParams)

docClass :: Parser ([String], [String])
docClass = cmdWithParams "documentclass"

specificBeginCtx :: SNatI n => String -> Vec n String -> Parser ([String], Vec (S n) String)
specificBeginCtx ctx ctxStack = do
    
   (optParams, [param]) <- cmdWithParams "begin"
   if param /= ctx 
     then P.parserFail $ "begin err. expected " <> ctx <> " found " <> param
     else return (optParams, ctx `Vec.cons` ctxStack)

endCtx :: SNatI n => Vec (S n) String -> Parser (Vec n String)
endCtx ctxStack = do
    
   (_, [param]) <- cmdWithParams "end"
   if param /= Vec.head ctxStack
     then P.parserFail "closing err."
     else return $ Vec.tail ctxStack
