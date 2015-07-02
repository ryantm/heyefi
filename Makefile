NIXSHELL_COMMAND=nix-shell \
    --pure \
    -j 4 \
    --show-trace \
    -I nixpkgs=/home/ryantm/p/nixpkgs \
    --option extra-binary-caches http://hydra.nixos.org \
    --option extra-binary-caches http://hydra.cryp.to \
    --option build-use-chroot true \
    --fallback

#    -p haskellngPackages.ghc haskellngPackages.cabal-install


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
