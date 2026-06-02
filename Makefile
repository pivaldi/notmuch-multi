# Makefile for notmuch-multi -- a thin wrapper around Eldev.
#
# Eldev resolves the `notmuch' dependency (from MELPA) and runs the ERT suite
# in test/ headlessly, so these targets work without any personal Emacs
# configuration.  Install Eldev first:
#   https://emacs-eldev.github.io/eldev/   (see "Installation")
#
# Override the binaries if they are not on PATH, e.g.:
#   make test EMACS=/usr/bin/emacs ELDEV=~/.local/bin/eldev

EMACS ?= emacs
ELDEV ?= eldev

.DEFAULT_GOAL := all
.PHONY: all test compile lint checkdoc prepare clean

## all: byte-compile, then run the test suite
all: compile test

## test: run the ERT suite in test/ (installs deps on first run)
test:
	$(ELDEV) -dtT test

## compile: byte-compile the package, surfacing warnings
compile:
	$(ELDEV) -dtT compile

## lint: run available linters (package-lint, relint, ...)
lint:
	$(ELDEV) lint

## checkdoc: docstring / package-convention check (no Eldev or deps needed)
checkdoc:
	$(EMACS) -Q --batch --eval '(checkdoc-file "notmuch-multi.el")'

## prepare: download/install dependencies into .eldev/ without testing
prepare:
	$(ELDEV) prepare

## clean: remove build artifacts and installed dependencies
clean:
	$(ELDEV) clean all
