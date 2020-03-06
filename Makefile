.PHONY: all

all: src/SSL/CA.pdf

# https://stackoverflow.com/questions/18178084/pandoc-and-foreign-characters
%.pdf : %.lhs
	pandoc $< -o $@ --pdf-engine=xelatex --template=my.latex \
                  --variable mainfont="DejaVu Serif" # --variable sansfont=Arial
