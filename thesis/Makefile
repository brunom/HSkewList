.DELETE_ON_ERROR:

all: thesis.pdf thesis.exe
pdf: thesis.pdf
exe: thesis.exe

thesis.pdf: thesis.tex thesis.bib
	latexmk -pdf -quiet thesis.tex

GHC=ghc-8.2.1
%GHC=ghc-7.4.2

thesis.exe : thesis.hs
	$(GHC) --make thesis.hs -o thesis.exe -freverse-errors

thesis.hs : thesis.lhs paper.lhs configuration.lhs
	lhs2TeX thesis.lhs -o thesis.hs --newcode

thesis.tex: records.lhs thesis.lhs paper.lhs
	lhs2TeX --poly thesis.lhs -o thesis.tex

clean:
	rm -f *.aux *.bbl *blg *.log *.ptb *.tex *.fls *.out *.fdb_latexmk thesis.pdf
	rm -f *.hi *.o
