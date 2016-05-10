# prepare the package for release
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: check clean

deps:
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

integration-need:
	git clone https://github.com/${TRAVIS_REPO_SLUG}-examples.git
	cd knitr-examples && \
		git checkout ${TRAVIS_BRANCH} && \
		GIT_PAGER=cat git show HEAD

integration-run:
	make deps xvfb-start knit xvfb-stop -C knitr-examples

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

# the svn mirror created by
# svn checkout svn+ssh://yihui@svn.r-forge.r-project.org/svnroot/isu/pkg/knitr knitr-rforge
svn:
	git archive master > ../knitr.tar;\
	cd ../knitr-rforge && rm -r `ls` && tar -xf ../knitr.tar;\
	svn add --force . && svn commit -m 'sync with git'

downstream:
	Rscript -e "source('http://developer.r-project.org/CRAN/Scripts/depends.R');" \
	-e "x = reverse_dependencies_with_maintainers('knitr', c('Depends', 'Imports', 'LinkingTo', 'Suggests'))" \
	-e "cat('\n'); cat(unique(x[, 'Maintainer']), sep = ', \n'); cat('\n')"

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

