
.PHONY: build
build: hasch

hasch: src/* app/*
	@ cabal install \
		--ghc \
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
	rm -f ./hasch
	rm -rf dist-newstyle
	rm -rf dist
	cabal clean

.PHONY: repl
repl: hasch
	@ ./hasch

.PHONY: benchmark
benchmark: hasch
	cd examples/the-little-schemer && \
		hyperfine -m 100 --warmup 10 \
			'gosch run-all.scm' \
			'scheme --quiet < run-all.scm' \
			'loco run-all.scm' \
			'../../../rusch/rusch run-all.scm' \
			'../../hasch run-all.scm'
