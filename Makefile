R_OPTS=--no-save --no-restore --no-init-file --no-site-file # vanilla, but with --environ

paper2.pdf: paper2.Rmd
	R ${R_OPTS} -e "rmarkdown::render('GBTB23.Rmd')"
	pdflatex paper2.tex
	bibtex paper2
	pdflatex paper2

clean:	paper2.pdf
	rm paper2.pdf

