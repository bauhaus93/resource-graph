.PHONY: all run clean

all: run

run: Main
	./Main

Main: *.hs
	ghc Main.hs

clean:
	rm -f *.hi *.o Main
