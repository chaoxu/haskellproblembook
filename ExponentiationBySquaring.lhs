\input{header}
%include polycode.fmt
%include greek.fmt
%include lambda.fmt
%include exists.fmt

\begin{document}
\section{Exponentiation by squaring}

\begin{problem}
\emph{Input}: 

Given associative operator $\cdot$, element $a$ and positive integer $n$.

\emph{Output}:

Find $a^n$, where $a^n = a\cdot a^{n-1}$.
\end{problem}

The general method to solve the problem is exponentiation by squaring. It is
originally used for integer exponentiation, but any associate operator can be
used in it's place. Here is a theorem stated in algebraic flavor.

\begin{theorem}
For any semigroup $(S,\cdot)$, $x\in S$ and $n\in \N$, $x^n$ can be computed 
with $O(\log n)$ applications of $\cdot$.
\end{theorem}

\begin{proof}
\begin{code}
import Data.Digits
exponentiationBySquaring :: Integral a => (b -> b -> b) -> b -> a -> b
exponentiationBySquaring op a n = foldr1 op $ snd . unzip $ filter (\(x,_) -> x /= 0) (zip binary twoPow)
  where twoPow = a:zipWith op twoPow twoPow
        binary = digitsRev 2 n
\end{code}

One can analyize the number of times the operator is used. The |twoPow| is the
infinite list $[a,a^2,\ldots,a^{2^i},\ldots$. It takes $k$ operations to
generate the first $k+1$ elements. At most $k$ additional operations are
required to combine the result with the operator. Therefore the operator is
used $O(\log n)$ times.
\end{proof}
\input{footer}
