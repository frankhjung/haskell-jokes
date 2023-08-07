#!/usr/bin/env make

SRC	:= $(shell git ls-files | grep --perl \.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

.PHONY: default
default: check build test

.PHONY: all
all:	check build test doc exec

.PHONY: check
check:	tags style lint

.PHONY: tags
tags:	$(SRC)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRC)

.PHONY: style
style:	$(SRC)
	@echo style ...
	@stylish-haskell --verbose --config=.stylish-haskell.yaml --inplace $(SRC)

.PHONY: lint
lint:	$(SRC)
	@echo lint ...
	@cabal check
	@hlint --cross --color --show $(SRC)
	@yamllint --strict $(YAML)

.PHONY: build
build:
	@echo build ...
	@stack build --pedantic --fast

.PHONY: test
test:
	@echo test ...
	@stack test --fast

.PHONY: doc
doc:
	@echo doc ...
	@stack haddock --no-haddock-deps Jokes

.PHONY: exec
exec:
	@stack exec -- main -

.PHONY: setup
setup:
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean

.PHONY: cleanall
cleanall: clean
	@stack purge
	@rm -f stack.yaml.lock
