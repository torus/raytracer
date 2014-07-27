APPNAME = 

SOURCES = index.scm raytracer.scm public

usage:
	@echo Usage: make release APPNAME=gauche-raytracer

heroku-login:
	heroku login

create-app:
	heroku create $(APPNAME)

extract-slug-id: slug.json
	echo `gosh extract-slug-id.scm < slug.json`

extract-slug-url: slug.json
	echo `gosh extract-slug-url.scm < slug.json`

slug.tgz: $(SOURCES) ../docker/gauche/app/gauche
	rm -rf app
	mkdir app
	cp -R ../docker/gauche/app/gauche app/gauche
	cp -R $(SOURCES) app/
	tar cfz $@ ./app

rebuild-slug:
	$(MAKE) -C ../docker/gauche

../docker/gauche/app/gauche:
	$(MAKE) -C ../docker/gauche

slug.json: slug.tgz
	[ "x$(APPNAME)" != "x" ]
	curl -X POST \
	-H 'Content-Type: application/json' \
	-H 'Accept: application/vnd.heroku+json; version=3' \
	-d '{"process_types":{"web":"./gauche/bin/gosh -I ./gauche/share/gauche-0.9/0.9.4/lib -I ./gauche/share/gauche-0.9/site/lib -I ./gauche/lib/gauche-0.9/0.9.4/x86_64-unknown-linux-gnu/ -I ./gauche/lib/gauche-0.9/site/x86_64-unknown-linux-gnu/ -I ./ index.scm --port=$$PORT"}}' \
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
	heroku config:set LD_LIBRARY_PATH=./gauche/lib --app=$(APPNAME)
	curl -X POST \
	-H "Accept: application/vnd.heroku+json; version=3" \
	-H "Content-Type: application/json" \
	-d '{"slug":"'`gosh extract-slug-id.scm < slug.json`'"}' \
	-n https://api.heroku.com/apps/$(APPNAME)/releases > $@

jpeg: result.jpg

%.jpg: %.ppm
	ppmtojpeg -quality 100 $^ > $@

result.ppm: raytracer.scm
	gosh raytracer.scm > $@

clean:
	rm -f *.jpg *.ppm out*.jpg release upload-slug slug.tgz slug.json *~
