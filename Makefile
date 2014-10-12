

.PHONE: init setup test

init:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests --reorder-goals
	cabal exec -- ghc-pkg expose hspec2
	cabal exec -- ghc-pkg expose hspec-snap

setup:
	cabal install --only-dependencies --enable-tests --reorder-goals


test:
	cabal exec -- runghc -isrc spec/Main.hs
