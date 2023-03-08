ghcver = 9.4.4
cabalver = 3.8.1.0

.PHONY: submission.cabal install.sh

artifacts/submission.cabal: \
		submission.cabal.template \
		dependencies
	mkdir -p artifacts
	sed -e 's/^/                  /' -e '$$!s/$$/,/' dependencies \
		> temp_dependencies
	sed -e '/REPLACE_CABAL_BUILD_DEPENDS/r temp_dependencies' \
		-e '//d' \
		submission.cabal.template > artifacts/submission.cabal
	rm temp_dependencies

submission.cabal: artifacts/submission.cabal

artifacts/install.sh: \
		artifacts/submission.cabal \
		cabal.project
	mkdir -p artifacts
	sed -e '/REPLACE_PKG_CABAL/r artifacts/submission.cabal' \
		-e '//d' \
		-e '/REPLACE_CABAL_PROJECT/r cabal.project' \
		-e '//d' \
		-e 's/REPLACE_GHCVER/$(ghcver)/' \
		-e 's/REPLACE_CABALVER/$(cabalver)/' \
		install.sh.template > artifacts/install.sh

install.sh: artifacts/install.sh