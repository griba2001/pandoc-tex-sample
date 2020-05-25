module BasicLatex (
  cmdWithParams,
  eol,
  text,
  inlineSpacing,
  eolBlanks,    
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, oneOf, noneOf, spaces, string, letter, alphaNum, newline, satisfy)
import Text.Parsec.Combinator (many1, sepBy1, option, optional, optionMaybe)
import Text.Parsec.Prim (many, skipMany, (<?>), (<|>), try)

import Data.Either (lefts, rights)

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
nl = newline >> return ()


inlineSpacing :: Parser ()
inlineSpacing = skipMany $ oneOf [' ', '\t']

eolBlanks :: Parser ()
eolBlanks = do
              inlineSpacing
              eolComment <|> nl
              return ()

eol :: Parser ()
eol = do
         many (nl <|> eolComment <|> try eolBlanks)
         return ()
         
namedParam :: Parser String
namedParam = many letter


-- LA(2) use with `try`
escapedClosingSqBracket :: Parser Char
escapedClosingSqBracket = do
            let ch = ']'
            char '{'
            char ch
            char '}'
            return ch

-- LA(2) use with `try`
escapedOpenSqBracket :: Parser Char
escapedOpenSqBracket = do
            let ch = '['
            char '{'
            char ch
            char '}'
            return ch

type EitherOptMandat = Either String String

-- LA(2) must avoid escapedOpenSqBracket, use with `try`
mandatParam :: Parser EitherOptMandat
mandatParam = do
               char '{'
               ch <- noneOf ['}', '[']  -- forbidden '['
               rest <- many $ noneOf ['}']
               char '}'
               return $ Right (ch : rest)

optParam :: Parser EitherOptMandat
optParam = do
            char '[' 
            str <- many (try escapedClosingSqBracket <|> noneOf [']'])
            char ']' 
            return $ Left str
            
cmdWithParams :: String -> Parser ([String], [String])           
cmdWithParams cmd = do
            try (bkslash >> string cmd) -- revert bkslash if it fails
            -- optional params may come before or after mandatory ones
            params <- many (try mandatParam <|> optParam)
            return (lefts params {- opts -}, rights params {- mandats -})

-- | first ch after cmd maybe an escapedOpenSqBracket           
text :: Parser String            
text = do
             let excepted = ['\\', '\n', '%'] 
             ch <- try escapedOpenSqBracket <|> noneOf excepted  
             txt <- many $ noneOf excepted
             return $ ch : txt
