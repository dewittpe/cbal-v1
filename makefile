ifeq ($(OS), Windows_NT)
	RM ?= del
else
  RM ?= /bin/rm
endif

PKG_ROOT    = .
PKG_VERSION = $(shell gawk '/^Version:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)
PKG_NAME    = $(shell gawk '/^Package:/{print $$2}' $(PKG_ROOT)/DESCRIPTION)

CRAN = "https://cran.rstudio.com"
BIOC = "https://bioconductor.org/packages/3.4/bioc"

SRC       = $(wildcard $(PKG_ROOT)/src/*.cpp)
RFILES    = $(wildcard $(PKG_ROOT)/R/*.R)
EXAMPLES  = $(wildcard $(PKG_ROOT)/examples/*.R)
TESTS     = $(wildcard $(PKG_ROOT)/tests/testthat/*.R)
VIGNETTES = $(wildcard $(PKG_ROOT)/vignettes/*.Rmd)
RAWDATAR  = $(wildcard $(PKG_ROOT)/data-raw/*.R)

.PHONY: all check install clean

all: $(PKG_NAME)_$(PKG_VERSION).tar.gz

.document.Rout: $(RFILES) $(SRC) $(EXAMPLES) $(RAWDATAR) $(VIGNETTES) $(PKG_ROOT)/DESCRIPTION
	if [ -e "$(PKG_ROOT)/data-raw/makefile" ]; then $(MAKE) -C $(PKG_ROOT)/data-raw/; else echo "Nothing to do"; fi
	Rscript --vanilla --quiet -e "options(repo = c('$(CRAN)', '$(BIOC)'))" \
		-e "options(warn = 2)" \
		-e "devtools::install_dev_deps()" \
		-e "devtools::document('$(PKG_ROOT)')" \
		-e "invisible(file.create('$(PKG_ROOT)/.document.Rout', showWarnings = FALSE))"

$(PKG_NAME)_$(PKG_VERSION).tar.gz: .document.Rout $(TESTS) $(PKG_ROOT)/DESCRIPTION
	R CMD build --no-resave-data --md5 $(build-options) $(PKG_ROOT)

check: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD check $(PKG_NAME)_$(PKG_VERSION).tar.gz

install: $(PKG_NAME)_$(PKG_VERSION).tar.gz
	R CMD INSTALL $(PKG_NAME)_$(PKG_VERSION).tar.gz

clean:
	$(RM) -f  $(PKG_NAME)_$(PKG_VERSION).tar.gz
	$(RM) -rf $(PKG_NAME).Rcheck
	$(RM) -f .document.Rout

