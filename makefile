

SRC=\
	src/Main.hs \
	src/UsageCLI.hs \
	src/Utils.hs

BIN=.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/pandoc-anki/pandoc-anki

all: $(BIN)

$(BIN): $(SRC)
	stack build .
	stack install .

# examples/%.pdf: examples/%.org $(BIN)
# 	stack exec pandoc-anki -- $<
# 	mv $*.pdf examples

# examples/%.png: examples/%.pdf makefile 
# 	convert -density 300 -quality 200 -delete 1--1 $< $@

# README.md: templates/readme.markdown examples/Category.org
# 	example=`cat examples/Category.org` envsubst < templates/readme.markdown > ./README.md


# clean:
# 	rm -f README.md examples/*.pdf examples/*.png
