# Commands:

.PHONY: build init test clean doc deploy stage

build:
	ghc --make -O -o cluster Main.hs

prof:
	ghc --make -prof -o cluster Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f cluster
	rm -f *.hi
	rm -f *.o

setup:
	if [ ! -d "/users/sfogarty" ]; then	cabal update; fi
	if [ ! -d "/users/sfogarty" ]; then	cabal v1-install ansi-terminal; fi
	if [ ! -d "/users/sfogarty" ]; then  cabal v1-install drawille; fi
