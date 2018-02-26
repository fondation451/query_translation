all:
	pdflatex report.tex && bibtex report.aux && pdflatex report.tex

clean:
	rm -rf *.aux *.log *.out *.toc *.nav *.snm *.bbl *.blg *.bib
	
read:all
	evince report.pdf
