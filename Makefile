GHC = stack ghc
GHCFLAGS = -- -O2 -Wall -fforce-recomp

default:
	$(GHC) $(GHCFLAGS) Main.hs
	rm -rf *.hi *.o


