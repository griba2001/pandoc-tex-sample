{-# LANGUAGE PackageImports, DataKinds #-}

{-| Here we parse a sample doc with a structure

-- hello-world.tex
\documentClass{article}
\begin{document}
Hello world

\begin{itemize}
 \item Pencil and watercolor sketch by Cassandra
 \item Rice portrait 
\end{itemize}

\end{document}
-- EOF--

Possible eol-comments are stripped at BasicLatex eol
-}

module LatexSimpleDoc (
  latexSimple,
  )    
    where
        
import BasicLatex (text, eol)
import LatexCmds (docClass, withNamedEnv, cmdItem)

import "parsec" Text.Parsec.String (Parser)
import "parsec" Text.Parsec.Prim ((<|>), (<?>))
import qualified "parsec" Text.Parsec.Char as P
import qualified "parsec" Text.Parsec.Combinator as P
import qualified "parsec" Text.Parsec.Prim as P

import "vec" Data.Vec.Pull (Vec)
import qualified "vec" Data.Vec.Pull as Vec

import "fin" Data.Type.Nat (Nat(Z, S), SNatI)

import Text.Pandoc.Definition (Pandoc(Pandoc))
import qualified Text.Pandoc.Definition as PD

inlineText :: Parser PD.Inline
inlineText = do
                txt <- text
                return $ PD.Str txt

-- | block elements parse to EndOfLine                
blockPara :: Parser PD.Block
blockPara = do
                inlines <- P.endBy1 inlineText eol
                return $ PD.Para inlines

blockPlain :: Parser PD.Block
blockPlain = do
                inlines <- P.endBy1 inlineText eol 
                return $ PD.Plain inlines
                
                
listItem :: Parser [PD.Block]
listItem = do
              mbLabel <- cmdItem
              item <- blockPlain
              case mbLabel of
                Just itemLabel -> return [PD.Plain [PD.Str itemLabel], item]
                Nothing -> return [item]

bulletListParser :: Vec n String -> [String] -> Parser PD.Block
bulletListParser _envStack _optParams = do
                items <- P.many listItem
                return $ PD.BulletList items

-- | parses the content of a document environment
docEnvParser :: SNatI n => Vec n String -> [String] -> Parser [PD.Block]
docEnvParser envStack _optParams = do
                blocks <- P.many (blockPlain 
                                  <|> withNamedEnv "itemize" envStack bulletListParser
                                  )
                return blocks
    
-- | parses the document prelude & document environment
latexSimple :: Parser Pandoc
latexSimple = do
                P.spaces
                docClass >> eol
                blocks <- withNamedEnv "document" Vec.empty docEnvParser
                return $ Pandoc PD.nullMeta blocks
  
