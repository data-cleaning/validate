
html: *.Rmd
	R -e 'bookdown::render_book("./")'

all: *.Rmd
	R -e 'bookdown::render_book("./index.Rmd", output_format="all")'

pdf: *.Rmd
	R -e 'bookdown::render_book("./", output_format=bookdown::pdf_document2())'
	xdg-open ../docs/validatecookbook.pdf

