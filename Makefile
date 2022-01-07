.PHONY: all
all: lint build
	@stack build --haddock --test --coverage --test-arguments '--rerun --failure-report=.stack-work/hspec-failed.log --rerun-all-on-success'

.PHONY: clean
clean:
	@stack clean

.PHONY: mrproper
mrproper:
	@stack purge

.PHONY: lint
lint:
	@hlint .

.PHONY: build
build:
	@stack build

.PHONY: test
test:
	@stack build --test

.PHONY: docs
docs:
	@stack build --haddock

.PHONY: publish
publish: build
	@cabal sdist

.PHONY: watch
watch:
	@while true; do \
		find . -not \( -path './.stack-work*' -or -path './.git*' \) \
			| entr -cd ${MAKE}; \
		[[ $$? == 0 ]] && break; \
	done
