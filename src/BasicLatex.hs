module BasicLatex (
  cmdWithParams,
  cmdWithNoParams,
  switchCmd,    
  eol,
  textLine,
  inlineSpacing,    
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, noneOf, spaces, string, letter, alphaNum, newline, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, option, optional)
import Text.Parsec.Prim (many, skipMany, (<?>), (<|>))

import Data.List (intercalate)

comma = char ',' :: Parser Char

bkslash = char '\\' :: Parser Char

nonNL :: Char -> Bool
nonNL '\n' = False
nonNL _ = True

eolComment :: Parser ()
eolComment = do
            char '%'
            skipMany (satisfy nonNL)
            newline
            return ()
            
nl :: Parser ()
nl = do
        newline
        return ()

eol :: Parser ()
eol = do
         many (eolComment <|> nl)
         return ()

inlineSpacing :: Parser ()
inlineSpacing = skipMany $ oneOf [' ', '\t']

namedParam :: Parser String
namedParam = many letter

alnumParam :: Parser String
alnumParam = fmap (intercalate " ") $ sepBy1 (many alphaNum) spaces 

mandatParams :: Parser [String]
mandatParams = do
               char '{'
               strs <- sepBy1 namedParam comma <?> "missing {param}"
               char '}'
               return strs

optParams :: Parser [String]
optParams = do
            char '[' 
            strs <- sepBy1 alnumParam comma <?> "missing [param]"
            char ']' 
            return strs
              
cmdWithParams :: String -> Parser ([String], [String])           
cmdWithParams cmd = do
            bkslash
            string cmd
            optPs <- option [] optParams
            params <- mandatParams <?> "missing mandatory {param}"
            return (optPs, params)

cmdWithNoParams :: String -> Parser ()           
cmdWithNoParams cmd = do
            bkslash
            string cmd
            return ()

switchCmd :: Parser String
switchCmd = do
            bkslash
            cmd <- many1 letter
            return cmd

textLine :: Parser String            
textLine = do
             txt <- many $ noneOf ['\\', '\n', '%']
             return txt
