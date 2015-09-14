NIX_PATH=nixpkgs=https://nixos.org/channels/nixpkgs-unstable/nixexprs.tar.xz
NIXSHELL_COMMAND=nix-shell \
    --pure \
    -j 4 \
    -I $(NIX_PATH) \
    --show-trace \
    --option extra-binary-caches https://hydra.nixos.org \
    --option extra-binary-caches https://cache.nixos.org \
    --fallback

build :
	$(NIXSHELL_COMMAND) --command "cabal configure && cabal build"

shell :
	$(NIXSHELL_COMMAND)

.PHONY : testv
testv :
	cabal exec -- runhaskell -Wall -Werror -fno-warn-orphans -isrc -itest test/Spec.hs

.PHONY : hlint
hlint :
	rm -f report.html
	cabal exec -- hlint lint src test --report

packunused :
	cabal update
	cabal clean
	rm -f *.imports
	cabal configure -O0 --disable-library-profiling
	cabal build --ghc-option=-ddump-minimal-imports
	packunused

setup_coverage :
	cabal clean
	cabal configure --enable-tests --enable-coverage
	cabal test
