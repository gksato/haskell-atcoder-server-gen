.DELETE_ON_ERROR:

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




# Clear all

.PHONY: clear-all
clear-all: ghcup/docker-image-clear serverproto/docker-image-clear
	rm -rf dist
	rm -rf tmp



# GHCup target

dist/ghcup/Dockerfile: \
		src/ghcup/Dockerfile
	mkdir -p dist/ghcup
	cp src/ghcup/Dockerfile dist/ghcup/Dockerfile


DUMMY := $(shell test ! -e dist/ghcup/docker-image-name || \
	(cat dist/ghcup/docker-image-name | xargs -I {} \
	docker image inspect {} > /dev/null 2>&1) || \
	rm -f dist/ghcup/docker-image-name)

dist/ghcup/docker-image-name: \
		dist/ghcup/Dockerfile
	echo "haskell-atcoder-server-gen/ghcup-autogen:$$(uuidgen)" \
	> dist/ghcup/docker-image-name

	cat dist/ghcup/docker-image-name

	docker image build \
	-t "$$(cat dist/ghcup/docker-image-name)" \
	--build-arg GHCVER="$(ghcver)" \
	--build-arg CABALVER="$(cabalver)" \
	--build-arg USERNAME="$(ghcup_user)" \
	--build-arg GHCUP_APT_DEPENDENCY="$(ghcup_apt_dependency)" \
	dist/ghcup

.PHONY: ghcup
ghcup: dist/ghcup/docker-image-name

.PHONY: ghcup/docker-image
ghcup/docker-image: dist/ghcup/docker-image-name

.PHONY: ghcup/docker-image-clear
ghcup/docker-image-unlink:
	rm -f dist/ghcup/docker-image-name

	@echo "No docker image for GHCup is connected to this Makefile \
	any longer, if there were any."

	@echo "This does not try to delete images, \
	so you have to do it yourself if you want to!"

	@echo "GHCup images have the following repository name:"

	@echo "  haskell-atcoder-server-gen/ghcup-autogen"


.PHONY: ghcup/clear
ghcup/clear: ghcup/docker-image-unlink



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


DUMMY := $(shell test ! -e dist/serverproto/docker-image-name || \
	docker image inspect $$(cat dist/serverproto/docker-image-name) \
	> /dev/null 2>&1 || \
	rm -f dist/serverproto/docker-image-name)


dist/serverproto/docker-image-name: \
		dist/serverproto/Dockerfile \
		dist/serverproto/submission.cabal \
		dist/serverproto/cabal.project \
		$(serverproto_main_dist) \
		dist/ghcup/docker-image-name
	echo "haskell-atcoder-server-gen/serverproto-autogen:$$(uuidgen)" \
	> dist/serverproto/docker-image-name

	docker image build \
	-t $$(cat dist/serverproto/docker-image-name) \
	--build-arg GHCUP_IMG="$$(cat dist/ghcup/docker-image-name)" \
	--build-arg DEFAULT_MAIN_FILE="$(serverproto_main_fname)" \
	--build-arg USERNAME="$(ghcup_user)" \
	dist/serverproto

.PHONY: serverproto
serverproto: dist/serverproto/docker-image-name

.PHONY: serverproto/docker-image
serverproto/docker-image: dist/serverproto/docker-image-name

.PHONY: serverproto/docker-image-clear
serverproto/docker-image-clear:
	rm -f dist/serverproto/docker-image-name

	@echo "No docker image for server prototype is connected to this \
	Makefile any longer, if there were any."
	@echo "This does not try to delete images, \
	so you have to do it yourself if you want to!"
	@echo "Server prototype images have the following repository name:"
	@echo "  haskell-atcoder-server-gen/serverproto-autogen"



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


# toolgen target

DUMMY := $(shell test ! -e dist/toolgen/build-container-id || \
	docker container inspect $$(cat dist/toolgen/build-container-id) \
	> /dev/null 2>&1 || \
	rm -f dist/ghcup/build-container-id)

.SECONDARY: dist/toolgen/build-container-id
dist/toolgen/build-container-id: \
		dist/ghcup/docker-image-name
	mkdir -p dist/toolgen
	docker container create -it \
	--name "haskell-atcoder-server-gen/toolgen-autogen:$$(uuidgen)" \
	$(shell cat dist/ghcup/docker-image-name) \
	> dist/toolgen/build-container-id

.PHONY: dist/toolgen/build-container
dist/toolgen/build-container: dist/toolgen/build-container-id

.PHONY: toolgen/build-container
toolgen/build-container: dist/toolgen/build-container-id

.SECONDARY: dist/toolgen/cabal-plan
dist/toolgen/cabal-plan: \
		dist/toolgen/build-container-id
	mkdir -p dist/toolgen
	docker container start \
	$(shell cat dist/toolgen/build-container-id)

	docker container exec --user=root \
	$(shell cat dist/toolgen/build-container-id) \
	apt-get install -y zlib1g-dev

	docker container exec \
	$(shell cat dist/toolgen/build-container-id) \
	cabal v2-install cabal-plan \
	--flag="+license-report" \
	--install-method=copy --overwrite-policy=always

	docker container cp \
	$(shell cat dist/toolgen/build-container-id)\
	:/home/$(ghcup_user)/.cabal/bin/cabal-plan \
	dist/toolgen/cabal-plan

	docker container stop \
	$(shell cat dist/toolgen/build-container-id)

.PHONY: toolgen/cabal-plan
toolgen/cabal-plan: dist/toolgen/cabal-plan

.PHONY: cabal-plan
cabal-plan: dist/toolgen/cabal-plan

CHECKSOURCEGEN_SOURCE := \
	$(shell find checksource-gen/src \
	checksource-gen/app \
	checksource-gen/test -name '*.hs')

dist/toolgen/checksource-gen: \
		dist/toolgen/build-container-id \
		$(CHECKSOURCEGEN_SOURCE) \
		checksource-gen/checksource-gen.cabal
	mkdir -p dist/toolgen
	docker container cp \
	checksource-gen/. \
	$(shell cat dist/toolgen/build-container-id)\
	:/home/$(ghcup_user)/checksource-gen

	docker container start \
	$(shell cat dist/toolgen/build-container-id)

	docker container exec --user=root \
	$(shell cat dist/toolgen/build-container-id) \
	chown -R $(ghcup_user):$(ghcup_user) \
	/home/$(ghcup_user)/checksource-gen

	docker container exec \
	$(shell cat dist/toolgen/build-container-id) \
	rm -rf /home/$(ghcup_user)/checksource-gen/dist-newstyle

	docker container exec \
	-w /home/$(ghcup_user)/checksource-gen \
	$(shell cat dist/toolgen/build-container-id) \
	cabal v2-install --install-method=copy \
	--overwrite-policy=always

	docker container cp \
	$(shell cat dist/toolgen/build-container-id)\
	:/home/$(ghcup_user)/.cabal/bin/checksource-gen \
	dist/toolgen/checksource-gen

	docker container stop \
	$(shell cat dist/toolgen/build-container-id)


.PHONY: donothing
donothing:

