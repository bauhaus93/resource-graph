.PHONY: all run clean

all: run

run: assembly_line
	./assembly_line

assembly_line: *.hs
	ghc -Wall -Weverything -o assembly_line *.hs

clean:
	rm -f *.hi *.o assembly_line
