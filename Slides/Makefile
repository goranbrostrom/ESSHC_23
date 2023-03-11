R_OPTS=--no-save --no-restore --no-init-file --no-site-file # vanilla, but with --environ

slides.pdf: slides.Rmd
	R ${R_OPTS} -e "library(rmarkdown);render('slides.Rmd')"
	xelatex slides.tex
