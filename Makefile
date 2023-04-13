#!/usr/bin/env make

SRC	:= $(wildcard *.hs **/*.hs)
YAML	:= $(shell git ls-files | grep --perl \.y?ml)

.PHONY: default
default:	check build test

.PHONY: check
check:	tags style lint

.PHONY: all
all:	check build exec

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

.PHONY: exec
exec:
	@echo exec ...
	@stack exec -- main

.PHONY: setup
setup:
	stack path
	stack query
	stack ls dependencies

.PHONY: clean
clean:
	@stack clean
	@cabal clean
	@$(RM) tags
	@$(RM) $(wildcard *.hi **/*.hi)
	@$(RM) $(wildcard *.o **/*.o)
	@$(RM) $(wildcard *.prof **/*.prof)
	@$(RM) $(wildcard *.tix **/*.tix)

.PHONY: cleanall
cleanall: clean
	@stack purge
