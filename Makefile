APPNAME = 

test: jpeg

heroku-login:
	heroku login

create-app:
	heroku create $(APPNAME)

extract-slug-id: slug.json
	echo `gosh extract-slug-id.scm < slug.json`

extract-slug-url: slug.json
	echo `gosh extract-slug-url.scm < slug.json`

slug.json:
	[ "x$(APPNAME)" != "x" ]
	curl -X POST \
	-H 'Content-Type: application/json' \
	-H 'Accept: application/vnd.heroku+json; version=3' \
	-d '{"process_types":{"web":"env LD_LIBRARY_PATH=./gauche/lib ./gauche/bin/gosh -I ./gauche/share/gauche-0.9/0.9.4_pre3/lib -I ./gauche/lib/gauche-0.9/0.9.4_pre3/x86_64-unknown-linux-gnu/ ./makiki/examples/basic.scm --port=$PORT"}}' \
	-n https://api.heroku.com/apps/$(APPNAME)/slugs > $@

jpeg: result.jpg

%.jpg: %.ppm
	ppmtojpeg -quality 100 $^ > $@

result.ppm: raytracer.scm
	gosh raytracer.scm > $@

clean:
	rm -f *.jpg *.ppm out*.jpg
