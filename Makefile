GHC = stack ghc
GHCFLAGS = -- -Wall -fforce-recomp

default:
	$(GHC) $(GHCFLAGS) Main.hs
	rm -rf *.hi *.o


