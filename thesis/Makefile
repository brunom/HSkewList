.DELETE_ON_ERROR:

all: thesis.pdf thesis.exe
pdf: thesis.pdf
exe: thesis.exe
run: thesis.hs
	run$(GHC) thesis.hs

thesis.pdf: thesis.tex thesis.bib
	latexmk -pdf thesis.tex

GHC=ghc-8.2.1
%GHC=ghc-7.4.2

thesis.exe : thesis.hs
	$(GHC) --make thesis.hs -o thesis.exe

thesis.hs : records.lhs thesis.lhs paper.lhs configuration.lhs
	lhs2TeX thesis.lhs -o thesis.hs --newcode

thesis.tex: records.lhs thesis.lhs paper.lhs configuration.lhs
	lhs2TeX --poly thesis.lhs -o thesis.tex

clean:
	rm -f *.aux *.bbl *blg *.log *.ptb *.tex *.fls *.out *.fdb_latexmk thesis.pdf thesis.hs
	rm -f *.hi *.o *.dyn_hi *.dyn_o
