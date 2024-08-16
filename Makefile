#!/usr/bin/env make

.DEFAULT_GOAL := default

CABAL	:= Jokes.cabal
SRCS	:= $(wildcard */*.hs)

.PHONY: default
default: format check build test exec

.PHONY: all
all:	setup format check build test doc exec

.PHONY: format
format:	$(SRCS)
	@echo format ...
	@cabal-fmt --inplace $(CABAL)
	@stylish-haskell --inplace $(SRCS)

.PHONY: check
check:	tags lint

.PHONY: tags
tags:	$(SRCS)
	@echo tags ...
	@hasktags --ctags --extendedctag $(SRCS)

.PHONY: lint
lint:	$(SRCS)
	@echo lint ...
	@hlint --cross --color --show $(SRCS)
	@cabal check

.PHONY: build
build:  $(SRCS)
	@echo build ...
	@cabal build

.PHONY: test
test:
	@echo test ...
	@cabal test --test-show-details=direct

.PHONY: doc
doc:
	@echo doc ...
	@cabal haddock --haddock-quickjump --haddock-hyperlink-source

.PHONY: exec
exec:
	@cabal run main

.PHONY: setup
setup:
	-touch -d "2023-04-12T000:00:00UTC" LICENSE
ifeq (,$(wildcard ${CABAL_CONFIG}))
	-cabal user-config init
	-cabal update --only-dependencies
else
	@echo Using user-config from ${CABAL_CONFIG} ...
endif

.PHONY: clean
clean:
	@cabal clean

.PHONY: distclean
distclean: clean
	@$(RM) tags
