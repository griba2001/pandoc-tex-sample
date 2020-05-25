{-# LANGUAGE PackageImports, QuasiQuotes #-}

module Samples where

import "raw-strings-qq" Text.RawString.QQ (r)

sample1, sample2, sample3 :: String

sample1 = [r|
\documentclass{article}

\begin{document}
Hello world
Hello again
\end{document}
|]


sample2 = [r|
\documentclass{article}
% this is a comment
\begin{document}
Hello world % this is a comment
Bye
\end{document}
|]


sample3 = [r|
\documentclass{article}

\begin{document}
Hello world % this is a comment

\begin{itemize}
\item Pencil
\item Pen
\end{itemize}

\end{document}
|]
