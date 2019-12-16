

validate.pdf: validate.tex jss3483.bib Figures/validate_uml_diagram.png Figures/validate_confrontation_uml.png
	pdflatex validate.tex 
	bibtex validate
	pdflatex validate.tex
	pdflatex validate.tex
	xdg-open validate.pdf


validate.tex: validate.Rnw
	R -e "options(prompt='R> '); Sweave('validate.Rnw')"

validate.R: validate.Rnw
	R -e "Stangle('validate.Rnw')"

clean:
	rm -rf *.aux *.tex *.bbl *.log *.out *.pdf

