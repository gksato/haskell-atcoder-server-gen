# Versions of GHC and cabal-install. See ghcup_apt_dependency.

ghcver = 9.4.4
cabalver = 3.8.1.0

# Write what GHCup says it needs.
# The exception to this is llvm: GHCup does not say it needs LLVM.
# However we need it to enable LLVM backend.
# The LLVM version GHC requires depends on GHC's version,
# so make sure adjust the version when you tweak GHC version!
ghcup_apt_dependency = build-essential curl libffi-dev libffi8ubuntu1 \
	libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm-13

# default username for docker images. THIS CANNOT BE root.
ghcup_user = runner

# Dummy files for Docker image.
# The file with name dummyname_*** exists,
# iff the docker image imgname_*** exists.
# The modification date of the former is the same as the creation date
# of the latter.

dummyname_ghcup = tmp/ghcup/.dockerimg_dummy
imgname_ghcup = haskell-atcoder-server-gen/ghcup:autogen
DUMMYVAR := $(shell ./touch_dummy_for_dockerimg.sh \
	"$(dummyname_ghcup)" "$(imgname_ghcup)")

dummyname_serverproto = tmp/serverproto/.dockerimg_dummy
imgname_serverproto = haskell-atcoder-server-gen/serverproto:autogen
DUMMYVAR := $(shell ./touch_dummy_for_dockerimg.sh \
	"$(dummyname_serverproto)" "$(imgname_serverproto)")


# Clear all

.PHONY: clear-all
clear-all: ghcup/docker-image-clear serverproto/docker-image-clear
	rm -rf dist
	rm -rf tmp



# GHCup target


$(dummyname_ghcup): \
		src/ghcup/Dockerfile
	docker image build \
		-t $(imgname_ghcup) \
		--build-arg GHCVER="$(ghcver)" \
		--build-arg CABALVER="$(cabalver)" \
		--build-arg USERNAME="$(ghcup_user)" \
		--build-arg GHCUP_APT_DEPENDENCY="$(ghcup_apt_dependency)" \
		src/ghcup
	@./touch_dummy_for_dockerimg.sh \
		"$(dummyname_ghcup)" "$(imgname_ghcup)"

.PHONY: ghcup
ghcup: $(dummyname_ghcup)

.PHONY: ghcup/docker-image
ghcup/docker-image: $(dummyname_ghcup)

.PHONY: ghcup/docker-image-clear
ghcup/docker-image-clear:
	docker image rm $(imgname_ghcup) || echo "No image to remove"
	@./touch_dummy_for_dockerimg.sh \
		"$(dummyname_ghcup)" "$(imgname_ghcup)"

.PHONY: ghcup/clear
ghcup/clear: ghcup/docker-image-clear



# serverproto targets


serverproto_main = src/serverproto/HelloHaskell.hs
serverproto_main_fname := $(notdir $(serverproto_main))
serverproto_main_dist := \
	$(addprefix dist/serverproto/,$(serverproto_main_fname))


$(serverproto_main_dist): \
		$(serverproto_main)
	@mkdir -p dist/serverproto
	cp $(serverproto_main) $(serverproto_main_dist)

.PHONY: $(serverproto_main_fname)
$(serverproto_main_fname): $(serverproto_main_dist)

.PHONY: $(addprefix serverproto/,$(serverproto_main_fname))
$(addprefix serverproto/,$(serverproto_main_fname)): $(serverproto_main_dist)



dist/serverproto/cabal.project: \
		src/serverproto/cabal.project
	@mkdir -p dist/serverproto
	cp src/serverproto/cabal.project dist/serverproto/cabal.project

.PHONY: cabal.project
cabal.project: dist/serverproto/cabal.project

.PHONY: serverproto/cabal.project
serverproto/cabal.project: dist/serverproto/cabal.project



dist/serverproto/submission.cabal: \
		src/serverproto/submission.cabal.template \
		src/serverproto/dependencies
	@mkdir -p tmp
	@mkdir -p dist/serverproto
	sed -e 's/^/                  /' -e '$$!s/$$/,/' \
		src/serverproto/dependencies \
		> tmp/build_depends
	sed -e '/REPLACE_CABAL_BUILD_DEPENDS/r tmp/build_depends' \
		-e '//d' \
		src/serverproto/submission.cabal.template \
		> dist/serverproto/submission.cabal
	rm tmp/build_depends

.PHONY: submission.cabal
submission.cabal: dist/serverproto/submission.cabal

.PHONY: serverproto/submission.cabal
serverproto/submission.cabal: dist/serverproto/submission.cabal



dist/serverproto/Dockerfile: \
		src/serverproto/Dockerfile
	mkdir -p dist/serverproto
	cp src/serverproto/Dockerfile dist/serverproto/Dockerfile

.PHONY: serverProto/Dockerfile
serverproto/Dockerfile: \
		dist/serverproto/Dockerfile



$(dummyname_serverproto): \
		dist/serverproto/Dockerfile \
		dist/serverproto/submission.cabal \
		dist/serverproto/cabal.project \
		$(serverproto_main_dist) \
		$(dummyname_ghcup)
	docker image build \
		-t $(imgname_serverproto) \
		--build-arg GHCUP_IMG="$(imgname_ghcup)" \
		--build-arg DEFAULT_MAIN_FILE="$(serverproto_main_fname)" \
		dist/serverproto
	@./touch_dummy_for_dockerimg.sh \
		"$(dummyname_serverproto)" "$(imgname_serverproto)"

.PHONY: serverproto
serverproto: $(dummyname_serverproto)

.PHONY: serverproto/docker-image
serverproto/docker-image: $(dummyname_serverproto)

.PHONY: serverproto/docker-image-clear
serverproto/docker-image-clear:
	docker image rm $(imgname_serverproto) || echo "No image to remove"
	@./touch_dummy_for_dockerimg.sh \
		"$(dummyname_serverproto)" "$(imgname_serverproto)"



.PHONY: serverproto/clear
serverproto/clear: serverproto/docker-image-clear
	rm -rf dist/serverproto




# installsteps targets

dist/installsteps/install.sh: \
		dist/serverproto/submission.cabal \
		dist/serverproto/cabal.project \
		src/installsteps/install.sh.template
	mkdir -p dist/installsteps
	sed -e '/REPLACE_CABAL_PROJECT/r dist/serverproto/cabal.project' \
		-e '//d' \
		-e '/REPLACE_PKG_CABAL/r dist/serverproto/submission.cabal' \
		-e '//d' \
		-e 's/REPLACE_GHCVER/$(ghcver)/' \
		-e 's/REPLACE_CABALVER/$(cabalver)/' \
		-e 's/REPLACE_GHCUP_APT_DEPENDENCY/$(ghcup_apt_dependency)/' \
		src/installsteps/install.sh.template \
		> dist/installsteps/install.sh

.PHONY: install.sh
install.sh: dist/installsteps/install.sh

.PHONY: installsteps/install.sh
installsteps/install.sh: dist/installsteps/install.sh

.PHONY: installsteps
installsteps: dist/installsteps/install.sh



.PHONY: installsteps/clear
installsteps/clear:
	rm -rf dist/installsteps



.PHONY: donothing
donothing:
