.PHONY: all

all: src/SSL/CA.pdf

# https://stackoverflow.com/questions/18178084/pandoc-and-foreign-characters
# to list available fonts: `fc-list :lang=en -v | less +/fullname`
# [see also|https://nixos.wiki/wiki/Fonts]
%.pdf : %.lhs
	pandoc $< -o $@ --pdf-engine=xelatex --template=my.latex \
                  --variable mainfont="DejaVu Serif"             \
	          --variable sansfont="DejaVu Sans"              \
	          --variable monofont="DejaVu Sans Mono"
