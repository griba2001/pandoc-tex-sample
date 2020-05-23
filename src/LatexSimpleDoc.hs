{-# LANGUAGE PackageImports #-}

{-| Here we parse a sample doc with a structure

-- hello-world.tex
\documentClass{article}
\begin{document}
Hello world  
\end{document}
-- EOF--

Possible eol-comments are stripped at BasicLatex
-}

module LatexSimpleDoc (
  latexSimple,
  )    
    where
        
import BasicLatex (textLine)
import LatexCmds (docClass, specificBeginCtx, endCtx)

import "parsec" Text.Parsec.String (Parser)
import qualified "parsec" Text.Parsec.Char as P
import qualified "parsec" Text.Parsec.Combinator as P
import qualified "parsec" Text.Parsec.Prim as P

import "vec" Data.Vec.Pull (Vec)
import qualified "vec" Data.Vec.Pull as Vec

import Text.Pandoc.Definition (Pandoc(Pandoc))
import qualified Text.Pandoc.Definition as PD


-- | parses a document context with a line of text
latexDocCtx :: Parser [PD.Block]
latexDocCtx = do
                -- specificBeginCtx pushes ctx into ctxStack as Vec
                (_optParams, ctxStack) <- specificBeginCtx "document" Vec.empty 
                P.newline
                P.spaces
                txt <- textLine
                P.newline
                _ <- endCtx ctxStack   -- pops ctx from ctxStack
                return $ [PD.Para [PD.Str txt]]
    
-- | parses the document prelude & document context
latexSimple :: Parser Pandoc
latexSimple = do
                P.spaces
                docClass
                P.newline
                P.spaces
                blocks <- latexDocCtx
                P.optional P.spaces
                return $ Pandoc PD.nullMeta blocks
  
