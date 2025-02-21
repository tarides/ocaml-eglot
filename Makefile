# Space-separated list of the dependencies of your project (include
# package-lint and/or buttercup if you want makel to use these tools):
ELPA_DEPENDENCIES=package-lint,flycheck

# List of package archives to download above dependencies
# from. Available archives are: gnu (aka elpa), elpa-devel, nongnu,
# melpa, melpa-stable and org:
ELPA_ARCHIVES=melpa gnu

# List of ERT test files:
TEST_ERT_FILES=$(wildcard test/*.el)

# List of files to check for Emacs conventions:
LINT_CHECKDOC_FILES=$(wildcard *.el) ${TEST_ERT_FILES}

# List of files to check for packaging guidelines:
LINT_PACKAGE_LINT_FILES=ocaml-eglot.el

# List of files to check for compilation errors and warnings:
LINT_COMPILE_FILES=${LINT_CHECKDOC_FILES}

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://github.com/DamienCassou/makel/raw/v0.8.0/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
