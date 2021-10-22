.PHONY: build clean run test

build:
	mkdir -p out
	raco exe -o out/machine-check main.rkt
	raco distribute out/usr out/machine-check

clean:
	rm -rf out

run:
	out/usr/bin/machine-check

test:
	raco test machine-check/test
