# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

deps:
	tlmgr install pgf preview xcolor;\
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.rstudio.com")'

docs:
	R -q -e 'library(Rd2roxygen); rab(".", build = FALSE)'

build:
	cd ..;\
	R CMD build --no-manual $(PKGSRC)

build-cran:
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build-cran
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

travis: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --no-manual

integration-run:
	xvfb-run make deps knit -C knitr-examples

integration-verify:
	GIT_PAGER=cat make diff -C knitr-examples

integration: install integration-run integration-verify

examples:
	cd inst/examples;\
	Rscript knit-all.R

vignettes:
	cd vignettes;\
	lyx -e knitr knitr-refcard.lyx;\
	sed -i '/\\usepackage{breakurl}/ d' knitr-refcard.Rnw;\
	mv knitr-refcard.Rnw assets/template-refcard.tex

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

