PDF: heather.pdf

HTML: html/heather.html

heather.pdf: heather.texinfo
	texi2pdf heather.texinfo

heather.info: heather.texinfo
	makeinfo heather.texinfo

html/heather.html: heather.texinfo
	if test ! -d html; then mkdir html; fi; \
	texi2html --output=html --noheader --nomenu --split=chapter heather.texinfo