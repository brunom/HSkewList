all: paper.pdf paper.exe

run : paper.exe
	time ./paper.exe

GHC=ghc-7.5.20120526
%GHC=ghc-7.4.1
%GHC=ghc-7.3.20111118
%GHC=ghc-7.0.4

paper.exe : paper.hs
	time $(GHC) -O -fcontext-stack=999 --make paper.hs -o paper.exe 2>&1

paper.hs : paper.lhs
	lhs2TeX paper.lhs -o paper.hs --newcode

paper.tex : paper.lhs
	lhs2TeX paper.lhs -o paper.tex

upperbound=4
LTX_OPTS=-halt-on-error

paper.pdf : paper.tex  biblio.bib
	pdflatex $(LTX_OPTS) paper.tex
	bibtex paper
	pdflatex $(LTX_OPTS) paper.tex

clean :
	rm -f *.aux *.bbl *blg *.log *.ptb *.tex paper.pdf
