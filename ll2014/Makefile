
index.html: text.md
	landslide presentation.cfg

open-pdf: ll2014-gauche-on-heroku.pdf
	open ll2014-gauche-on-heroku.pdf

%.dvi: %.tex
	/usr/texbin/platex $^

%.pdf: %.dvi
	/usr/texbin/dvipdfmx $^
