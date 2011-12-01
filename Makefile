all:
	lhs2TeX -o main.tex main.lhs
	pdflatex --interaction=nonstopmode main.tex
#	bibtex main
	pdflatex --interaction=nonstopmode main.tex
#	pdflatex --interaction=nonstopmode main.tex
	rm main.tex main.aux main.bbl main.blg main.log main.out main.ptb main.toc
