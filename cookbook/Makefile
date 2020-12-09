
html: *.Rmd
	R -e 'bookdown::render_book("./")'

pdf: *.Rmd
	R -e 'bookdown::render_book("./", output_format=bookdown::pdf_document2(latex_engine="xelatex"))'
	xdg-open ../docs/validatecookbook.pdf

