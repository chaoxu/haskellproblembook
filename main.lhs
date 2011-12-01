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
%\usepackage[x11names, rgb]{xcolor}
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
\newtheorem{problem}{Problem}[section]
\newtheorem{definition}{Definition}[section]
\newtheorem{conjecture}{Conjecture}[section]
\newtheorem{example}{Example}[section]

\theoremstyle{remark}
\newtheorem*{remark}{Remark}
\newtheorem*{note}{Note}

%opening

\title{Cookbook}
\date{Last update: \today}
\begin{document}
\maketitle
\setcounter{tocdepth}{1}
\tableofcontents
\chapter{Algebraic Algorithms}
\section{Exponentiation by squaring}

\begin{problem}
~\newline
\noindent \emph{Input}:

Given the operator $\cdot$, element $a$ and positive integer $n$. Where $a$ is
an element of a semigroup under $\cdot$.

\noindent \emph{Output}:

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
\emph{Input}: 
\begin{enumerate}
\item A list of coefficients $[c_1,c_2,\ldots,c_n]$ of a linear recurrence relation.
\item A list of base cases $[a_0,a_1,\ldots,a_{n-1}]$ of a linear recurrence relation.
\end{enumerate}

\emph{Output}:
The sequence of values of the linear recurrence relation as a infinite list $[a_0,a_1,\ldots$.
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

\chapter{Combinatorial Algorithms}
\section{Integer Partitions}
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
\bibliography{bib}
\bibliographystyle{plain}
\end{document}
