{-# LANGUAGE PackageImports #-}

module Main where

import "pandoc-tex" Lib (latexSimple)

import "pandoc-types" Text.Pandoc.Definition (Pandoc(Pandoc))
import qualified "pandoc-types" Text.Pandoc.Definition as PD

import "parsec" Text.Parsec (runParser)

import "hspec" Test.Hspec (hspec, describe, it, shouldBe)

import Samples (sample1, sample2)

main :: IO ()
main = hspec $ do         
          describe "Latex Hello world" $ do
              it "parses sample1" $ do
                  (runParser latexSimple () "sample1" sample1) `shouldBe` 
                      (Right $ Pandoc PD.nullMeta [PD.Para [PD.Str "Hello world"]])
                  
              it "parses sample2" $ do
                  (runParser latexSimple () "sample2" sample2) `shouldBe` 
                      (Right $ Pandoc PD.nullMeta [PD.Para [PD.Str "Hello world "]])
