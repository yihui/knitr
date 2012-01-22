# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= knitr-intro.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2pdf('$*.Rnw')"
