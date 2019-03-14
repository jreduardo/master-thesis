slides: slides-thesis.Rnw slides-figure/*

	Rscript -e 'knitr::knit("slides-thesis.Rnw")'
	/usr/bin/pdflatex slides-thesis.tex
	-bibtex slides-thesis.aux
	/usr/bin/pdflatex slides-thesis.tex
	/usr/bin/pdflatex slides-thesis.tex
	make -s clean

simple:
	Rscript -e 'knitr::knit("slides-thesis.Rnw")'
	/usr/bin/pdflatex slides-thesis.tex

clean:
	rm -f *.aux *.bbl *.blg *.brf *.idx *.ilg *.ind *.lof *.log \
	.*lol *.lot *.out *.toc *.synctex.gz *.nav *.snm *.vrb
