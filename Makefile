

no-gui: icfpc2021.cabal
	cabal v1-configure -O0
	cabal v1-build --jobs=12

gui: icfpc2021.cabal
	cabal v1-configure -f gui -O0
	cabal v1-build --jobs=12

icfpc2021.cabal: package.yaml
	hpack --force

clean:
	cabal v1-clean
