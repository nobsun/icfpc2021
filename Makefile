
oflag = -O0
jobs = --jobs=12

no-gui: icfpc2021.cabal
	cabal v1-configure $(oflag)
	cabal v1-build $(jobs)

gui: icfpc2021.cabal
	cabal v1-configure $(oflag) -f gui
	cabal v1-build $(jobs)

check: icfpc2021.cabal
	cabal v1-configure $(oflag) --enable-tests
	cabal v1-build $(jobs)
	cabal v1-test

v2-check: icfpc2021.cabal
	cabal v2-configure $(oflag) --enable-tests
	cabal v2-build $(jobs)
	cabal v2-test

icfpc2021.cabal: package.yaml
	hpack --force

clean:
	cabal v1-clean

v2-clean:
	cabal v2-clean
