

SRC ?= \
  $(wildcard src/*.hs) \
  $(wildcard src/Deck/*.hs) \
  $(wildcard src/Deck/Crowd/*.hs)

BIN=.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/pandoc-anki/pandoc-anki

show:
	echo $(SRC)

.PHONY: install
install:
	stack install

.PHONY: test
test: $(BIN)
	$(BIN) ./examples/Algebra.org -j

$(BIN): $(SRC)
	stack build .

pandoc-anki.json: $(BIN)
	$(BIN) ./examples/Algebra.org -j > $@

# cd src && stack ghc -- Deck/Crowd/Test.hs -e "d_dump dr" > ../pandoc-anki.json

# README.md: templates/readme.markdown examples/Category.org
# 	example=`cat examples/Category.org` envsubst < templates/readme.markdown > ./README.md

# clean:
# 	rm -f README.md examples/*.pdf examples/*.png
