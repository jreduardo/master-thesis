project: eduardo-quali.R eduardo-quali.Rnw ../bibliography.bib

	Rscript -e 'knitr::knit("eduardo-quali.Rnw")'
	xelatex eduardo-quali.tex
	-bibtex eduardo-quali.aux
	xelatex eduardo-quali.tex
	xelatex eduardo-quali.tex
	make -s clean

slides: slides-quali.Rnw ../bibliography.bib

	Rscript -e 'knitr::knit("slides-quali.Rnw")'
	xelatex slides-quali.tex
	-bibtex slides-quali.aux
	xelatex slides-quali.tex
	xelatex slides-quali.tex
	make -s clean

simple:
	Rscript -e 'knitr::knit("eduardo-quali.Rnw")'
	xelatex eduardo-quali.tex

clean:
	rm -f *.aux *.bbl *.blg *.brf *.idx *.ilg *.ind *.lof *.log \
	.*lol *.lot *.out *.toc *.synctex.gz
