APPNAME = 

SOURCES = index.scm raytracer.scm worker.scm public

usage:
	@echo Usage: make release APPNAME=gauche-raytracer-16

heroku-login:
	heroku login

create-app:
	heroku create $(APPNAME)

extract-slug-id: slug.json
	echo `gosh extract-slug-id.scm < slug.json`

extract-slug-url: slug.json
	echo `gosh extract-slug-url.scm < slug.json`

slug.tgz: $(SOURCES) ./gauche-heroku-cedar/app/gauche
	rm -rf app
	mkdir app
	cp -R ./gauche-heroku-cedar/app/gauche app/gauche
	cp -R $(SOURCES) app/
	tar cfz $@ ./app

rebuild-slug:
	$(MAKE) -C ./gauche-heroku-cedar

./gauche-heroku-cedar/app/gauche:
	$(MAKE) -C ./gauche-heroku-cedar

slug.json: slug.tgz
	[ "x$(APPNAME)" != "x" ]
	curl -X POST \
	-H 'Content-Type: application/json' \
	-H 'Accept: application/vnd.heroku+json; version=3' \
	-d '{"process_types":{"web":"./gauche/bin/gosh -I ./gauche/share/gauche-0.9/0.9.6_pre6/lib -I ./gauche/share/gauche-0.9/site/lib -I ./gauche/lib/gauche-0.9/0.9.6_pre6/x86_64-pc-linux-gnu/ -I ./gauche/lib/gauche-0.9/site/x86_64-pc-linux-gnu/ -I ./ index.scm --port=$$PORT --num-threads=$$NUM_THREADS","worker":"./gauche/bin/gosh -I ./gauche/share/gauche-0.9/0.9.6_pre6/lib -I ./gauche/share/gauche-0.9/site/lib -I ./gauche/lib/gauche-0.9/0.9.6_pre6/x86_64-pc-linux-gnu/ -I ./gauche/lib/gauche-0.9/site/x86_64-pc-linux-gnu/ -I ./ worker.scm"}}' \
	-n https://api.heroku.com/apps/$(APPNAME)/slugs > $@

upload-slug: slug.json
	curl -X PUT \
	-H "Content-Type:" \
	--data-binary @slug.tgz \
	`gosh extract-slug-url.scm < slug.json`
	gosh extract-slug-url.scm < slug.json > $@
	date >> $@

release: upload-slug
	[ "x$(APPNAME)" != "x" ]
	heroku config:set LD_LIBRARY_PATH=./gauche/lib NUM_THREADS=1 --app=$(APPNAME)
	curl -X POST \
	-H "Accept: application/vnd.heroku+json; version=3" \
	-H "Content-Type: application/json" \
	-d '{"slug":"'`gosh extract-slug-id.scm < slug.json`'"}' \
	-n https://api.heroku.com/apps/$(APPNAME)/releases > $@

jpeg: result.jpg

%.jpg: %.ppm
	ppmtojpeg -quality 100 $^ > $@

result.ppm: raytracer.scm
	gosh raytracer.scm --size=64 48 --frame=0 0 64 48 > $@

clean:
	rm -f *.jpg *.ppm out*.jpg release upload-slug slug.tgz slug.json *~
