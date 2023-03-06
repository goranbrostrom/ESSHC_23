R_OPTS=--no-save --no-restore --no-init-file --no-site-file # vanilla, but with --environ

GBTB23.pdf: GBTB23.Rmd
	R ${R_OPTS} -e "rmarkdown::render('GBTB23.Rmd')"
	pdflatex GBTB23.tex
	bibtex GBTB23
	pdflatex GBTB23

clean:	GBTB23.pdf
	rm GBTB23.pdf

