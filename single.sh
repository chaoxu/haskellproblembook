#!/bin/sh
lhs2TeX -o $1.tex $1.lhs
pdflatex --interaction=nonstopmode $1.tex
bibtex $1
pdflatex --interaction=nonstopmode $1.tex
pdflatex --interaction=nonstopmode $1.tex
rm $1.tex $1.aux $1.bbl $1.blg $1.log $1.out $1.ptb
