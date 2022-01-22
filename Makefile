.PHONY: build clean run test

build:
	mkdir -p out
	raco exe -o out/machine-check main.rkt
	raco distribute out/usr out/machine-check

clean:
	rm -rf out
	rm -rf compiled
	rm -rf machine-check/compiled
	rm -rf machine-check/test/compiled

run: clean
	racket main.rkt

docs: clean
	scribble --html +m --redirect-main http://docs.racket-lang.org/ --dest docs scribblings/index.scrbl

test:
	raco test machine-check/test
