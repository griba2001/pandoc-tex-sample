{-# LANGUAGE PackageImports, DataKinds #-}

{-| Here we parse a sample doc with a structure

-- hello-world.tex
\documentClass{article}
\begin{document}
Hello world  
\end{document}
-- EOF--

Possible eol-comments are stripped at BasicLatex eol
-}

module LatexSimpleDoc (
  latexSimple,
  )    
    where
        
import BasicLatex (textLine, eol)
import LatexCmds (docClass, withNamedEnv)

import "parsec" Text.Parsec.String (Parser)
import qualified "parsec" Text.Parsec.Char as P
import qualified "parsec" Text.Parsec.Combinator as P
import qualified "parsec" Text.Parsec.Prim as P

import "vec" Data.Vec.Pull (Vec)
import qualified "vec" Data.Vec.Pull as Vec

import "fin" Data.Type.Nat (Nat(Z, S))

import Text.Pandoc.Definition (Pandoc(Pandoc))
import qualified Text.Pandoc.Definition as PD


-- | parses the content of a document environment
docEnvParser :: Vec (S Z) String -> [String] -> Parser [PD.Block]
docEnvParser _envStack _optParams = do
                txt <- textLine
                return $ [PD.Para [PD.Str txt]]
    
-- | parses the document prelude & document environment
latexSimple :: Parser Pandoc
latexSimple = do
                P.spaces
                docClass
                eol
                blocks <- withNamedEnv "document" Vec.empty docEnvParser
                P.optional P.newline
                return $ Pandoc PD.nullMeta blocks
  
