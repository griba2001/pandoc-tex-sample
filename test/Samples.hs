{-# LANGUAGE PackageImports, QuasiQuotes #-}

module Samples where

import "raw-strings-qq" Text.RawString.QQ (r)

sample1 :: String
sample1 = [r|
\documentclass{article}

\begin{document}
Hello world
\end{document}
|]

sample2 :: String
sample2 = [r|
\documentclass{article}

\begin{document}
Hello world% this is a comment
\end{document}
|]
