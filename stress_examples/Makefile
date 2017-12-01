COMPILER=../inplace/bin/ghc-stage1
FLAGS=-rtsopts -debug -eventlog -threaded -dcore-lint

EXAMPLES_SRC=$(wildcard ./*.hs)
EXAMPLES_OUT=$(patsubst %.hs, %, $(EXAMPLES_SRC))
EXAMPLES_HI=$(patsubst %.hs, %.hi, $(EXAMPLES_SRC))
EXAMPLES_O=$(patsubst %.hs, %.o, $(EXAMPLES_SRC))

# run with: ./prog +RTS -M1200000000

all: $(EXAMPLES_OUT)

clean:
	rm -f $(EXAMPLES_OUT) $(EXAMPLES_HI) $(EXAMPLES_O)

%: %.hs
	$(COMPILER) $(FLAGS) $<
