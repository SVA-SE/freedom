# Determine package name and version from DESCRIPTION file
PKG_VERSION=$(shell grep -i ^version DESCRIPTION | cut -d : -d \  -f 2)
PKG_NAME=$(shell grep -i ^package DESCRIPTION | cut -d : -d \  -f 2)

# Name of built package
PKG_TAR=$(PKG_NAME)_$(PKG_VERSION).tar.gz

# Install package
install: build
	cd .. && R CMD INSTALL $(PKG_TAR)

# Build documentation with roxygen
# 1) Check version of roxygen2 before building documentation
# 2) Remove old doc
# 3) Generate documentation
roxygen:
	rm -f man/*.Rd
	cd .. && Rscript -e "library(roxygen2); roxygenize('$(PKG_NAME)')"

# Generate PDF output from the Rd sources
# 1) Rebuild documentation with roxygen
# 2) Generate pdf, overwrites output file if it exists
pdf: roxygen
	cd .. && R CMD Rd2pdf --force $(PKG_NAME)

# Build and check package
check: clean
	cd .. && R CMD build --no-build-vignettes $(PKG_NAME)
	cd .. && R CMD check --no-manual --no-vignettes --no-build-vignettes $(PKG_TAR)

# Build package
build: clean
	cd .. && R CMD build $(PKG_NAME)

# Build and check package
check_full: clean
	cd .. && R CMD build $(PKG_NAME)
	cd .. && R CMD check --as-cran $(PKG_TAR)

clean:
	-rm -f src/*.o
	-rm -f src/*.so
	-rm -rf src-x64
	-rm -rf src-i386

.PHONY: install roxygen pdf check clean
