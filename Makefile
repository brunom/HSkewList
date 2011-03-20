all: paper.pdf paper.hs

paper.hs : paper.lhs
	lhs2Tex paper.lhs -o paper.hs --newcode

paper.tex : paper.lhs
	lhs2TeX paper.lhs -o paper.tex

upperbound=4
LTX_OPTS=-halt-on-error

paper.pdf : paper.tex  biblio.bib
	latex $(LTX_OPTS) paper.tex
	latex $(LTX_OPTS) paper.tex
	bibtex paper
	pdflatex $(LTX_OPTS) paper.tex

clean :
	rm -f *.aux *.bbl *blg *.log *.ptb *.tex *.pdf
