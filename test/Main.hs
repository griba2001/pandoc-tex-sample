{-# LANGUAGE PackageImports, AllowAmbiguousTypes #-}

module Main where

import "pandoc-tex" Lib (latexSimple)

import "pandoc-types" Text.Pandoc.Definition (Pandoc(Pandoc))
import qualified "pandoc-types" Text.Pandoc.Definition as PD

import "parsec" Text.Parsec (runParser)

import "hspec" Test.Hspec (SpecWith, Arg, Example, hspec, describe, it, shouldBe)

import GHC.Stack (HasCallStack)

import Samples (sample1, sample2, sample3)


parsesSample1, parsesSample2, parsesSample3 :: HasCallStack => SpecWith (Arg Bool) 

parsesSample1 = it "parses sample1" $ do
                  (runParser latexSimple () "sample1" sample1) `shouldBe` 
                      (Right $ Pandoc PD.nullMeta [PD.Plain [PD.Str "Hello world", PD.Str "Hello again"]])

parsesSample2 = it "parses sample2" $ do                      
                  (runParser latexSimple () "sample2" sample2) `shouldBe` 
                      (Right $ Pandoc PD.nullMeta [PD.Plain [PD.Str "Hello world ", PD.Str "Bye"]])
    
parsesSample3 = it "parses sample3" $ do
                  (runParser latexSimple () "sample3" sample3) `shouldBe` 
                      (Right $ Pandoc PD.nullMeta [PD.Plain [PD.Str "Hello world "],
                                                   PD.BulletList [
                                                      [PD.Plain [PD.Str " Pencil"]],
                                                      [PD.Plain [PD.Str " Pen"]]
                                                      ] 
                                                   ])   
    
main :: IO ()
main = hspec $ do         
          describe "Latex Hello world" $ do
              parsesSample1
              parsesSample2
              parsesSample3
