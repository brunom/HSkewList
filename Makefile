all: Paper.pdf Paper.exe

run : Paper.exe
	time ./Paper.exe

GHC=ghc
%GHC=ghc-7.6.1
%GHC=ghc-7.4.2

Paper.exe : Paper.hs
	time $(GHC) -O -fcontext-stack=999 --make Paper.hs -o Paper.exe 2>&1

Paper.hs : Paper.lhs
	lhs2TeX Paper.lhs -o Paper.hs --newcode

Paper.tex : Paper.lhs
	lhs2TeX Paper.lhs -o Paper.tex

upperbound=4
LTX_OPTS=-halt-on-error

Paper.pdf : Paper.tex  biblio.bib
	pdflatex $(LTX_OPTS) Paper.tex
	bibtex Paper
	pdflatex $(LTX_OPTS) Paper.tex

clean :
	rm -f *.aux *.bbl *blg *.log *.ptb *.tex Paper.pdf
