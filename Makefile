
hasch: src/* app/*
	@ cabal install \
		--install-method=copy \
		--installdir=dist \
		--enable-optimization=2 \
		--disable-debug-info \
		--disable-library-for-ghci \
		--enable-executable-stripping \
		--enable-library-stripping \
		--enable-relocatable \
		--overwrite-policy=always
	@ cp dist/hasch .
	@ rm -rf dist

.PHONY: test
test: unit-test integration-test

.PHONY: unit-test
unit-test:
	@ cabal test \
		--verbose=1 \
		--test-show-details=always

.PHONY: integration-test
integration-test: hasch
	cd examples/the-little-schemer && ../../hasch run-all.scm

.PHONY: run
run:
	cabal run hasch

.PHONY: lines
lines:
	@ find . -type f \( -name "*.hs" -not -path "./tests/*" -not -path "./dist-*/*" \) \
		 -exec cat {} \; | grep . | wc -l

.PHONY: clean
clean:
	rm -rf dist
	rm -rf hasch
	cabal clean

.PHONY: repl
repl: hasch
	@ ./hasch
