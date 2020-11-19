
all: *.Rmd
	R -e 'bookdown::render_book("./")'

pdf: *.Rmd
	R -e 'bookdown::render_book("./", output_format=bookdown::pdf_document2())'
	xdg-open _book/validatecookbook.pdf

