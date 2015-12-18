all: Main

Main:
	ghc Main.hs

clean-haskell:
	rm -rf *.o *.hi

clean:
	rm -rf *.o *.hi Main
