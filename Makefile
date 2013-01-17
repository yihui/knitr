# prepare the package for release
NEWS     = NEWS
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename $(PWD))

all: news check clean

# convert markdown to R's NEWS format
news: $(NEWS)
	sed -e 's/^-/  -/' -e 's/^## *//' -e 's/^#/\t\t/' <NEWS.md | fmt -80 >$(NEWS)

deps:
	Rscript -e 'if (!require("Rd2roxygen")) install.packages("Rd2roxygen", repos="http://cran.r-project.org")'

docs:
	R -q -e 'library(Rd2roxygen); rab(".", build = FALSE)'

build: docs
	cd ..;\
	R CMD build $(PKGSRC)

install: build
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz --as-cran

examples:
	cd inst/examples;\
	Rscript knit-all.R

vignettes:
	cd inst/doc;\
	lyx -e knitr knitr-intro.lyx;\
	lyx -e knitr knitr-refcard.lyx

# the svn mirror created by
# svn checkout svn+ssh://yihui@svn.r-forge.r-project.org/svnroot/isu/pkg/knitr
# commit everything to R-Forge
svn:
	git archive master > ../../svn/knitr.tar;\
	cd ../../svn/knitr && rm -r `ls` && tar -xf ../knitr.tar;\
	svn add --force . && svn commit -m 'sync with git'

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

