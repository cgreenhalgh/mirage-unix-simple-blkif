default: build

config:
	ocaml setup.ml -configure --enable-tests

build:
	ocaml setup.ml -build

clean:
	ocaml setup.ml -clean
	${RM} -f testfile

test: build
	dd bs=4K count=256 if=/dev/zero of=testfile
	./blkdev_test.native 

