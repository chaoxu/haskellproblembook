\documentclass[oneside]{book}
%include polycode.fmt
%include greek.fmt
%include lambda.fmt
%include exists.fmt
%include style.fmt
\usepackage{enumerate}
\usepackage{amsmath}
\usepackage{amssymb}
\usepackage{amsfonts}
\usepackage{amsthm}
\usepackage{latexsym}
\usepackage{mathrsfs}
\usepackage[usenames,dvipsnames]{color}
\usepackage{mathrsfs}
\usepackage{boxedminipage}
\usepackage[utf8]{inputenc}
\usepackage{tikz}
\usepackage{multicol}
\usepackage{hyperref}
\usepackage{cite}
\usepackage{stmaryrd}
\setlength{\topmargin}{-0.5in} \setlength{\textwidth}{6.5in}
\setlength{\oddsidemargin}{0.0in} \setlength{\textheight}{9.1in}


\newlength{\pagewidth}
\setlength{\pagewidth}{6.5in} %\pagestyle{empty}
\pagenumbering{arabic}
\newcommand{\R}{\mathbb{R}}
\newcommand{\N}{\mathbb{N}}
\newcommand{\B}{\mathfrak{B}}
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\lcm}{\mathop{\mathrm{lcm}}}
\newcommand{\sgn}{\mathop{\mathrm{sgn}}}
\newcommand{\topics}[1]{[\textcolor{CornflowerBlue}{\emph{#1}}]\\}

\newcommand{\catamorphism}[1]{\llparenthesis #1 \rrparenthesis}
\newcommand{\hylomorphism}[1]{[|#1 |]}
\newcommand{\anamorphism}[1]{[(#1 ])}
\newcommand{\paramorphism}[1]{#1}

\theoremstyle{plain}
\newtheorem{theorem}{Theorem}[section]
\newtheorem{lemma}[theorem]{Lemma}
\newtheorem{proposition}[theorem]{Proposition}
\newtheorem{corollary}[theorem]{Corollary}

\theoremstyle{definition}
\newtheorem{pbm}{Problem}[section]
\newtheorem{definition}{Definition}[section]
\newtheorem{conjecture}{Conjecture}[section]
\newtheorem{example}{Example}[section]

\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{note}{Note}

\newenvironment{problem}{
\noindent \begin{boxedminipage}{6.5in}
\begin{pbm}
~\\
}
{
\end{pbm}
	\end{boxedminipage}
}
\newenvironment{boxsection}[1]{
\textbf{\emph{#1}}

}{}
%opening

\title{Algorithmic Recipes in Haskell}
\date{Last update: \today}
\author{Chao Xu}
\begin{document}
\maketitle
\setcounter{tocdepth}{1}
\tableofcontents
\chapter{Exact Numerical Algorithms}
\section{Continued Fraction Representation}
Continued fractions can be used to represent real numbers in a much more
natrual way than decimal notations. In fact, the arithmetic operations are not
difficult for continued fractions.

A continued fraction can be implemented as a list of integers. There is one
operation that works for all arithmetics of continued fraction. All other ones
can be derived.

\subsection{The main arithmetic operation}
The algorithm is derived by Bill Gosper. The algorithm here is directly
modeled after is Mark Jason Dominus's talk.

\href{https://github.com/Mgccl/mgccl-haskell/blob/master/random/ContinuedFraction.hs}{Link of current implementation}.
\chapter{Algebraic Algorithms}
\section{Exponentiation by squaring}

\begin{problem}
\begin{boxsection}{Input}
Given the operator $\cdot$, element $a$ and positive integer $n$. Where $a$ is
an element of a semigroup under $\cdot$.
\end{boxsection}

\begin{boxsection}{Output}
Find $a^n$, where $a^n = a\cdot a^{n-1}$.
\end{boxsection}
\end{problem}

The general method to solve the problem is exponentiation by squaring. It is
originally used for integer exponentiation, but any associate operator can be
used in it's place. Here is a theorem stated in algebraic flavor.

\begin{theorem}
For any semigroup $(S,\cdot)$, $x\in S$ and $n\in \N$, $x^n$ can be computed 
with $O(\log n)$ applications of $\cdot$.
\end{theorem}

\begin{proof}
Express $n$ as binary $c_kc_{k-1}\ldots c_0$, where $c_i\in \{0,1\}$. We
make sure $0\cdot a$ is the treated as the identity, and $1\cdot a = a$ for
all $a$. The following observations are crucial.
\begin{align*}
a^n &= c_0a^{2^0}\cdot c_1a^{2^1}\cdot \ldots \cdot c_ka^{2^k}\\
a^{2^{i+1}} &= a^{2^i}\cdot a^{2^i}
\end{align*}

The code that compute $a^n$ from the above two equalities.

\begin{code}
import Data.Digits
exponentiationBySquaring :: Integral a => (b -> b -> b) -> b -> a -> b
exponentiationBySquaring op a n = foldr1 op $ [y | (x,y) <- (zip binary twoPow), x/=0]
  where twoPow = a:zipWith op twoPow twoPow
        binary = digitsRev 2 n
\end{code}

One can analyize the number of times the operator is used. The |twoPow| is the
infinite list $[a,a^2,\ldots,a^{2^i},\ldots$. It takes $k$ operations to
generate the first $k+1$ elements. At most $k$ additional operations are
required to combine the result with the operator. Therefore the operator is
used $O(\log n)$ times.
\end{proof}

This result can of course be extended to monoid and groups, so it work for all
non-negative and integer exponents, respectively.

\section{Linear homogeneous recurrence relations with constant coefficients}

\begin{definition}[Linear homogeneous recurrence relations with constant
coefficients]
A linear homogeneous recurrence relations in ring $R$ with constant coefficients of order
$k$ is a sequence with the following recursive relation
\[
a_n = \sum_{i=1}^k c_ia_{n-i}
\],
where $c_i$ are constants.

We use linear recurrence relation to abbreviate.
\end{definition}

The most common example is the Fibonacci sequence. $F_0=0, F_1=1$ and $F_n =
F_{n-1}+F_{n-2}$ in the ring $\Z$. The Fibonacci sequence have a simple implementation. 
|fibs = 0 : 1 : zipWith (+) fibs (tail fibs)|. We want to generalize it.

\subsection{Lazy sequence}
\begin{problem}
\begin{boxsection}{Input}
\begin{enumerate}
\item A list of coefficients $[c_1,c_2,\ldots,c_n]$ of a linear recurrence relation.
\item A list of base cases $[a_0,a_1,\ldots,a_{n-1}]$ of a linear recurrence relation.
\end{enumerate}
\end{boxsection}

\begin{boxsection}{Output}
The sequence of values of the linear recurrence relation as a infinite list $[a_0,a_1,\ldots$.
\end{boxsection}
\end{problem}

Here is a specific implementation where we are working in the ring $\Z$.
\begin{code}
import Data.List
linearRecurrence :: Integral a => [a] -> [a] -> [a]

linearRecurrence coef base = a
  where a = base ++ map (sum . (zipWith (*) coef)) (map (take n) (tails a))
        n = (length coef)
\end{code}

One can generalize it easily to any ring. 

Having a infinite list allows simple manipulations. However, finding the nth
element in the sequence cost $O(nk)$ time. It becomes unreasonable if a person
only need to know the $n$th element.

\subsection{Determine $n$th element in the index}
If $n$ is very large, a more common technique would be solve for $a_n$ using
matrix multiplication.

\subsection{Linear Recurrence in Finite Ring}
Linear recurrence is perodic in finite rings. Therefore one might want to
produce only the periodic part of the ring. [INSERT MORE ON THIS SUBJECT]

\section{A particular kind of recurrence}
\label{nicerec}
A common recurrence has the form 
\[
a_n = \sum_{i=0}^\infty b_ia_{n-m_i}
\]
, where $m_i$ and $b_i$ are both \emph{infinite} sequences. $m_i\in \N$. $a_{-i}=0$ 
for all positive $i$. This is well defined as long as $b_i, a_i$ are in the
	same ring.

\begin{problem}
\begin{boxsection}{Input}
Infinite sequence $b_i$ and $m_i$, $m_i\in \N$. Finite sequence $c_0,\ldots,c_k$
\end{boxsection}

\begin{boxsection}{Output}
The infinite sequence defined as

\[
a_n = \begin{cases}
   \sum_{i=0}^\infty b_ia_{n-m_i} &\text{if }n>k\\
   c_n & \text{if }n\leq k
	   \end{cases}
\]
\end{boxsection}
\end{problem}

One can use a balanced binary tree to store the entire infinite list, and 
the time to generate the $n$th element is $O(d(n)\log n)$, where $d$ is the 
density function of $\{m_i\}$.

Using an array would make it $O(d(n))$, but it is too imperative for our taste, 
how about we only use list and achieve $O(d(n))$ time, elegantly?

The idea is that we are summing the first item of infinite many stacks. However we
don't have to really sum the infinite stacks, we only sum the stack we
require.

\begin{code}
import Data.List
rec :: Num a => [a] -> [a] -> [Int] -> [a]
rec c b m = a
  where a    = c++rest
        rest = next [] 0 m
        next xs k (m:ms) 
		  | k == m    = next (a:xs) k ms
          | otherwise = val ++ next (map tail xs) (k+1) (m:ms)
          where val = if (k<length c) then [] else [sum $ zipWith (*) (reverse (map head xs)) b]
\end{code}

This kind of problem can be thought of moving the pointer of to $a_n$ from
$a_{n-1}$, how does the pointers that originally pointing to all the elements
$a_{n-1}$ requires to sum should move to now? This is a extra complication
required when an array is not accessable.

\section{Canonical forms of a boolean function}
One can always describe a boolean function $f$ over $n$ variables to a list of
$2^n$ boolean values, by mapping it into a function $g$.

\[
f(x_n,\ldots, x_1) = g(\sum_{i=1}^n 2^{i-1}x_i)
\]

\subsection{Sum of minterms}
\begin{problem}
\begin{boxsection}{Input}
A list of values of $\{0,1\}$, of length $2^n$.
\end{boxsection}

\begin{boxsection}{Output}
Find a function of the following form that also generate the same list.
\[
\bigvee_{y\in Y} (\bigwedge_{x\in y} x)
\]
Where each $x$ is either $x_i$ or $\neg x_i$ for some $i$. 
$Y$ can be described by a list of lists. Use $i$ to denote
$x_i$ and $-i$ to denote $x_i$.

\end{boxsection}
\end{problem}

\subsection{Product of maxterms}
\begin{problem}
\begin{boxsection}{Input}
Input the same as the sum of minterms.
\end{boxsection}

\begin{boxsection}{Output}
Find a function of the following form that also generate the same list.
\[
\bigwedge_{y\in Y} (\bigvee_{x\in y} x)
\]
Output $Y$.
\end{boxsection}

\end{problem}

\subsection{Implementation}
\begin{code}
import Data.Digits
import Data.List
import Data.List.Utils

sumOfMinterms     = snd.booleanCanonicalForm
productOfMaxterms = fst.booleanCanonicalForm

booleanCanonicalForm :: Integral a => [a] -> ([[a]], [[a]])
booleanCanonicalForm values = (snd $ unzip pos, snd $ unzip sop)
               where (pos,sop) = partition (\(x,y) -> x==0) (zip values power)
                     power     = map (terms . (replace [0] [-1]) . pad . digitsRev 2) [0..]
                     terms d   = zipWith (*) d [1..]
                     pad a     = a ++ replicate (n-(length a)) 0
                     n         = floor $ logBase 2 (fromIntegral (length values))
\end{code}


\chapter{Combinatorial Algorithms}
\section{List of Lattice Points}

\begin{problem}
\begin{boxsection}{Input}
Positive integer $k$.
\end{boxsection}

\begin{boxsection}{Output}
A infinite list that contain all nonnegative lattice points in $k$-th
dimension.
\end{boxsection}

\end{problem}

\begin{code}
nonNegativeLatticePoints k = concat $ map (sumToN k) [0..]
  where sumToN k n 
          | k == 1    = [[n]]
          | otherwise = concat [(map (i:) (sumToN (k-1) (n-i))) | i<-[0..n]]
\end{code}

\begin{problem}
\begin{boxsection}{Input}
Positive integer $k$.
\end{boxsection}

\begin{boxsection}{Output}
A infinite list that contain all lattice points in $k$-th
dimension.
\end{boxsection}

\end{problem}

To show an example, here is list of integers.

\begin{code}
integers :: [Integer]
integers = (0:)$ concat $ zipWith (\x y -> [x,y]) [1..] (map negate [1..])
\end{code}

One want a way to be able to list all elements in the $k$-th dimension.
\section{Integer Partitions}

\begin{definition}[Integer Partition]
A integer partition of $n$ is a multiset $\{a_1,\ldots,a_k\}$, such that
$\sum_{i=1}^k a_i = n$.
\end{definition}

\begin{definition}[Partition Numbers]
The sequence of partition numbers $\{p(n)\}$ is the number of integer
partitions for $n$.
\end{definition}


\begin{problem}
\begin{boxsection}{Input}
Integer $n$.
\end{boxsection}

\begin{boxsection}{Output}
List of partitions of $n$.
\end{boxsection}

\end{problem}

To find all possible partition of a integer, we proceed with a simple
recursive formula.

Let $p(n,k)$ be the list of ways to partition integer $n$ using integers less
or equal to $k$. $p(n,n)$ is the solution to our problem. It is implemented as
$part$ in the code.
\begin{code}
integerPartitions :: Integral a => a -> [a]
integerPartitions n = part n n
  where part 0 _ = [[]]
        part n k = [(i:is) | i<-[1..min k n], is <- part (n-i) i]
\end{code}

\begin{problem}
\begin{boxsection}{Input}
None
\end{boxsection}

\begin{boxsection}{Output}
The infinite list of partition numbers.
\end{boxsection}

\end{problem}

Naively, |0:map (length . integerPartitions) [1..]| works well, except the
time complexity is $O(np(n))$, and $p(n)$ is exponential. A more
well known approach, that only cost $O(\sqrt{n})$ additional operations to 
generate the $n$th number, will be given instead. 

Extend the definition of the partition number, such that $p(0)=1$ 
and $p(-n)=0$ for all positive integer $n$. The partition number $p(n)$ has
the relation
\[
p(n) = \sum_{k=0}^\infty (-1)^k (p(n-p_{2k+1})+p(n-p_{2k+2}))
\]
where $p_n$ is the sequence of generalized pentagonal number.

We have already developed the tools to work with this kind of recurrence in
section \ref{nicerec}.
\begin{code}


generalizedPentagonalNumbers :: [Integer]
generalizedPentagonalNumbers = [(3 * n^2 - n) `div` 2|n<-integers]

partitionNumbers :: [Integer]
partitionNumbers = rec [1] (cycle [1,1,-1,-1]) (tail generalizedPentagonalNumbers)
\end{code}



\section{Find the primitive word in a free monoid}
\begin{problem}

\begin{boxsection}{Input}
A word $w$ in a free monid.
\end{boxsection}

\begin{boxsection}{Output}
A primitive word $p$, such that $p^n=w$ for some integer $n$.
\end{boxsection}

\end{problem}
A word $p$ is primitive if $p=w^k$ implies $k=1$.
This will use the algorithm in \cite{Czumaj00onthe}. [Nah, just KMP...]

\section{Period of a eventually periodic sequence}
A sequence is eventually periodic if it is a concatination of a finite
sequence and a periodic sequence.
\subsection{Knows the upper bound of the period and a certain condition}
\begin{problem}
\begin{boxsection}{Input}
\begin{enumerate}
  \item A integer of the upper bound $u$ of the period.
  \item A infinite list that represent a eventually periodic sequence, such
  that if two finite sequence of length $u$ are equal and the starting index is less than 
  $u$ apart, then they must be inside the periodic part of the sequence.
\end{enumerate} 
\end{boxsection}

\begin{boxsection}{Output}
  A pair of the initial sequence and the periodic part.
\end{boxsection}
\end{problem}

The naive algorithm, for each finite sequence of length $u$, see if the second
condition in the input holds, does pretty well if $u$ is small. In fact
$O((n+u)u^2)$ where $n$ is the length of the aperiodic part.

\begin{code}
import Data.List
import Data.Maybe
eventuallyPeriodic :: Eq a => [a] -> Int -> ([a], [a])
eventuallyPeriodic sequence bound = (ini,take period rep)
  where table      = map (take (bound+1)) (tails (map (take bound) (tails sequence)))
        exist      = map (\x-> elemIndex (head x) (tail x)) table
        period     = 1 + (fromJust $ head just)
        (no, just) = span isNothing exist
        (ini, rep) = splitAt (length no) sequence
\end{code}

Of course it can be improved to $O((n+u)u)$ easily by using a smarter string
search algorithm like KMP. Under a simple observation $O(n+u)$ is the
possible.

The algorithm can be abstracted as another sequence. Given a sequence $a$ in
the problem, and a $u$, we can define another sequence $b$, such that $b_i$ is
true if and only if $a_i$ meets condition two. $b_i = Fasle,\ldots,False,True,\ldots$, and the $False$ correspond to the finite
part. In this sense, it become obvious a binary search would suffice, and one
can construct a solution in $O(n+u + u\log(n+u))$ time.

To make it truly $O(n+u)$, we need to get $u\log(n+u) = O(n+u)$. How so, when
we don't even know what $n$ is? Only when $n$ is very small would
$u\log(n+u)>n$. Consider the following hackish algorithm:

$O(n+u)$ is clearly the lowerbound, one must read to the $n+u$th position in
the sequence to be able to decide the periodic part.

Check if the condition is true for $b_{ku}$, where $k$ is a integer. 
After $(n+u)/u+1$ tests are required figure out which $u$ positions can be 
the start of the periodic sequence. We know this can be done in
$O((n+u)/u\times u) = O(n+u)$ time. 

This the problem really reduce to can we find a the first substring of length
$u$ that appears twice in a string of length $2u$ in $O(u)$ time.

A variation of the problem could be the upper bound for length of the non-periodic 
part of the sequence is known.

\subsection{Upper bound of the length of the non-periodic part and
upper bound of the period are known}
\begin{problem}
\begin{boxsection}{Input}
\begin{enumerate}
  \item A infinite list that represent a eventually periodic sequence.
  \item A integer of the upper bound $u$ of the period.
  \item A integer $n$ represent the upper bound on the length of the
  non-periodic part of the sequence.
\end{enumerate} 
\end{boxsection}

\begin{boxsection}{Output}
  A pair of the initial sequence and the periodic part.
\end{boxsection}
\end{problem}

\chapter{Other Recipes}
\section{Simulate a biased coin}

\section{Random element from a finite discrete distribution}
\begin{problem}
\begin{boxsection}{Input}
Probability distribution of $n$ events as $p_1/q_1,\ldots,p_n/q_n$.
\end{boxsection}

\begin{boxsection}{Query}
\begin{enumerate}
\item input null, randomly pick an event depend on the distribution.
\end{enumerate}
\end{boxsection}
\end{problem}
This can be done with $O(n)$ preprocessing time, $O(n)$ space and
$O(\log Q)$, where $Q = \max_{1\leq i\leq n} \{q_i\}$.
\bibliography{bib}
\bibliographystyle{plain}
\end{document}
