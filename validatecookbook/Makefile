
all: *.Rmd
	R -e 'bookdown::render_book("./")'

pdf: *.Rmd
	R -e 'bookdown::render_book("./", output_format="pdf_document")'

