EXECUTABLE=assembly_line
CFLAGS=-Wall -Weverything
GRAPH_FILE=graph.png

.PHONY: all run clean

all: run

rebuild: clean $(EXECUTABLE)

run: $(EXECUTABLE)
	./$(EXECUTABLE)

$(GRAPH_FILE): $(EXECUTABLE)
	./$(EXECUTABLE) | dot -T png -o $(GRAPH_FILE)

show: $(GRAPH_FILE)
	feh --scale-down $(GRAPH_FILE)

assembly_line: *.hs
	ghc $(CFLAGS) -o $(EXECUTABLE)  *.hs

clean:
	rm -f *.hi *.o $(EXECUTABLE) $(GRAPH_FILE)
