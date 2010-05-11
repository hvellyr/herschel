destdir = $(HOME)/texmf
mapnm = OcrB.map
fontnm = ocrb.ttf
afmnm = OcrB.afm

fonts:
	if [ -f "$(mapnm)" ]; then \
  	rm -f $(mapnm); \
	fi
	ttf2afm -e 8r.enc -o $(afmnm) $(fontnm)
	afm2tfm $(afmnm) -v fobr.vpl -T 8r.enc rfobr.tfm >> $(mapnm)
	vptovf fobr.vpl fobr.vf fobr.tfm
	afm2tfm $(afmnm) -s 0.16 -v fobro.vpl -T 8r.enc rfobro.tfm >> $(mapnm)
	vptovf fobro.vpl fobro.vf fobro.tfm
	cat OcrB.map | sed -e 's(" <8r.enc(" <$(destdir)/fonts/ttf/mo/ocr-b/$(fontnm) <8r.enc(g' > tmp.0000
	mv tmp.0000 $(mapnm)

dist:
	if [ ! -d "$(destdir)/fonts/tfm" ]; then mkdir $(destdir)/fonts/tfm; fi
	if [ ! -d "$(destdir)/fonts/tfm/mo" ]; then mkdir $(destdir)/fonts/tfm/mo; fi
	if [ ! -d "$(destdir)/fonts/tfm/mo/ocr-b" ]; then mkdir $(destdir)/fonts/tfm/mo/ocr-b; fi

	if [ ! -d "$(destdir)/fonts/vf" ]; then mkdir $(destdir)/fonts/vf; fi
	if [ ! -d "$(destdir)/fonts/vf/mo" ]; then mkdir $(destdir)/fonts/vf/mo; fi
	if [ ! -d "$(destdir)/fonts/vf/mo/ocr-b" ]; then mkdir $(destdir)/fonts/vf/mo/ocr-b; fi

	if [ ! -d "$(destdir)/fonts/afm" ]; then mkdir $(destdir)/fonts/afm; fi
	if [ ! -d "$(destdir)/fonts/afm/mo" ]; then mkdir $(destdir)/fonts/afm/mo; fi
	if [ ! -d "$(destdir)/fonts/afm/mo/ocr-b" ]; then mkdir $(destdir)/fonts/afm/mo/ocr-b; fi

	if [ ! -d "$(destdir)/fonts/ttf" ]; then mkdir $(destdir)/fonts/ttf; fi
	if [ ! -d "$(destdir)/fonts/ttf/mo" ]; then mkdir $(destdir)/fonts/ttf/mo; fi
	if [ ! -d "$(destdir)/fonts/ttf/mo/ocr-b" ]; then mkdir $(destdir)/fonts/ttf/mo/ocr-b; fi

	if [ ! -d "$(destdir)/fonts/map" ]; then mkdir $(destdir)/fonts/map; fi
	if [ ! -d "$(destdir)/fonts/map/dvips" ]; then mkdir $(destdir)/fonts/map/dvips; fi
	if [ ! -d "$(destdir)/fonts/map/dvips/mo" ]; then mkdir $(destdir)/fonts/map/dvips/mo; fi
	if [ ! -d "$(destdir)/fonts/map/dvips/mo/ocr-b" ]; then mkdir $(destdir)/fonts/map/dvips/mo/ocr-b; fi

	cp *.tfm $(destdir)/fonts/tfm/mo/ocr-b/
	cp *.afm $(destdir)/fonts/afm/mo/ocr-b/
	cp *.vf $(destdir)/fonts/vf/mo/ocr-b/
	cp *.ttf $(destdir)/fonts/ttf/mo/ocr-b/
	cp *.map $(destdir)/fonts/map/dvips/mo/ocr-b/

	sudo texhash
	sudo updmap


