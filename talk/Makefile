upperbound=4
LTX_OPTS=-halt-on-error

main.pdf : main.lhs
	lhs2TeX main.lhs -o main.tex
	latexmk -pdf main.tex
