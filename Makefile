CFLAGS=-Wall -Weverything
GRAPH_FILE=graph.png

SRC_FILES_FACTORIO=src/App/Factorio/Factorio.hs \
	src/App/Factorio/Facilities.hs \
	src/App/Factorio/Names.hs \
	src/App/Factorio/Recipe/Assembly.hs \
	src/App/Factorio/Recipe/Chemical.hs \
	src/App/Factorio/Recipe/Furnance.hs \
	src/App/Factorio/Recipe/Recipe.hs

SRC_FILES=src/Main.hs \
		  src/App/Draw.hs \
		  src/App/Facility.hs \
		  src/App/Factory.hs \
		  src/App/Graph.hs \
		  src/App/ProductionChain.hs \
		  src/App/Recipe.hs \
		  src/App/Resource.hs \
		  src/App/Throughput.hs \
		  $(SRC_FILES_FACTORIO)

BUILD_DIR=build
EXECUTABLE_NAME=assembly_line
EXECUTABLE_PATH=$(BUILD_DIR)/$(EXECUTABLE_NAME)

.PHONY: all run clean

all: run

rebuild: clean $(EXECUTABLE_PATH)

run: $(EXECUTABLE_PATH)
	./$(EXECUTABLE_PATH)

$(GRAPH_FILE): $(EXECUTABLE_PATH)
	./$(EXECUTABLE_PATH) | dot -T png -o $(GRAPH_FILE)

show: $(GRAPH_FILE)
	feh --scale-down $(GRAPH_FILE)

$(EXECUTABLE_PATH): $(SRC_FILES)
	ghc $(CFLAGS) -outputdir $(BUILD_DIR) -o $(EXECUTABLE_PATH)  $(SRC_FILES)

clean:
	rm -f $(BUILD_DIR)/* $(GRAPH_FILE)
