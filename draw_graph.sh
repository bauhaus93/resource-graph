#/bin/sh

cabal run --verbose=0 | dot -T png -o graph.png && feh --scale-down graph.png
