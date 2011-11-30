\input{header}
%include polycode.fmt
%include greek.fmt
%include lambda.fmt
%include exists.fmt

\begin{document}
\section{Linear homogeneous recurrence relations with constant coefficients}

\begin{definition}[Linear homogeneous recurrence relations with constant
coefficients]
A linear homogeneous recurrence relations with constant coefficients of order
$k$ is a sequence with the following recursive relation
\[
a_n = \sum_{i=1}^k c_ia_{n-i}
\],
where $c_i$ are constants.

We use linear recurrence relation to abbreviate.
\end{definition}

The most common example is the Fibonacci sequence. $a_0=0, a_1=1$ and $a_n =
a_{n-1}+a_{n-2}$. The Fibonacci sequence have a simple implementation. |fibs = 0 : 1 : zipWith
(+) fibs (tail fibs)|. We want to generalize it.

\subsection{Lazy sequence}
\begin{problem}
\emph{Input}: 
\begin{enumerate}
\item A list of coefficients $[c_1,c_2,\ldots,c_n]$ of a linear recurrence relation.
\item A list of base cases $[a_0,a_1,\ldots,a_{n-1}]$ of a linear recurrence relation.
\end{enumerate}

\emph{Output}:
The sequence of values of the linear recurrence relation as a infinite list $[a_0,a_1,\ldots$.
\end{problem}

\begin{code}
import Data.List
linearRecurrence :: Num a => [a] -> [a] -> [a]

linearRecurrence coef base = a
  where a = base ++ map (sum . (zipWith (*) coef)) (map (take n) (tails a))
        n = (length coef)
\end{code}

Having a infinite list allows simple manipulations. However, finding the nth
number in the sequence cost $O(nk)$ time. It becomes unreasonable if a person
only need to know the $n$th number.

\subsection{Determine $n$th element in the index}
If $n$ is very large, a more common technique would be solve for $a_n$ using
matrix multiplication.
\input{footer}
