jpag: result.jpg

%.jpg: %.ppm
	ppmtojpeg -quality 100 $^ > $@

result.ppm: raytracer.scm
	gosh raytracer.scm > $@
