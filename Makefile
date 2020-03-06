.PHONY: all

all: src/X509.pdf

%.pdf : %.lhs
	pandoc $^ -o $@
