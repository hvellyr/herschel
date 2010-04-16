pdf: heather.pdf

html: html/heather.html

info: heather.info

heather.pdf: heather.texinfo texinfo.tex
	texi2pdf heather.texinfo

heather.info: heather.texinfo texinfo.tex
	makeinfo heather.texinfo

html/heather.html: heather.texinfo texinfo.tex
	if test ! -d html; then mkdir html; fi; \
	texi2html --output=html --noheader --nomenu --split=chapter heather.texinfo