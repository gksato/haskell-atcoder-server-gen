ghcver = 9.4.4
cabalver = 3.8.1.0
imgname = haskell-atcoder-server-gen/artifact-server-proto:latest
# imgname = gksato/ghc-on-ubuntu:9.4.4

DATEFMT := $(shell date -j > /dev/null && \
	echo "date -u -j -f '%Y-%m-%d %H:%M:%S %z'" \
	|| echo "date -u -d")
IMGDATE := $(shell docker image ls --format "{{.CreatedAt}}" $(imgname))

ifeq ($(IMGDATE),)
DUMMY := $(shell rm -f tmp/dockerimg)
else
FMTDATE := $(shell $(DATEFMT) "$(IMGDATE)" +"%Y-%m-%dT%H:%M:%SZ")
DUMMY := $(shell mkdir -p tmp && touch -d "$(FMTDATE)" tmp/dockerimg)
endif


.PHONY: submission.cabal install.sh \
	dockerimg dockerimg-rm dockerimg-rebuild

artifacts/submission.cabal: \
		submission.cabal.template \
		dependencies
	mkdir -p artifacts
	mkdir -p tmp
	sed -e 's/^/                  /' -e '$$!s/$$/,/' dependencies \
		> tmp/build_depends
	sed -e '/REPLACE_CABAL_BUILD_DEPENDS/r tmp/build_depends' \
		-e '//d' \
		submission.cabal.template > artifacts/submission.cabal
	rm tmp/build_depends

submission.cabal: artifacts/submission.cabal

artifacts/install.sh: \
		artifacts/submission.cabal \
		cabal.project
	sed -e '/REPLACE_PKG_CABAL/r artifacts/submission.cabal' \
		-e '//d' \
		-e '/REPLACE_CABAL_PROJECT/r cabal.project' \
		-e '//d' \
		-e 's/REPLACE_GHCVER/$(ghcver)/' \
		-e 's/REPLACE_CABALVER/$(cabalver)/' \
		install.sh.template > artifacts/install.sh

install.sh: artifacts/install.sh

tmp/dockerimg: \
		artifacts/submission.cabal \
		cabal.project \
		Dockerfile
	docker image build -t $(imgname) .
	mkdir -p tmp
	touch tmp/dockerimg

dockerimg: tmp/dockerimg

dockerimg-rm:
	docker image rm $(imgname)
	rm -f tmp/dockerimg

