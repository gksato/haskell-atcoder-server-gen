FROM ubuntu:22.10

ARG GHCVER="9.4.4"
ARG CABALVER="3.8.1.0"
ARG MAIN_FILE="HelloHaskell.hs"
ARG CABAL_FILE="artifacts/submission.cabal"
ARG PROJ_FILE="cabal.project"
RUN apt-get update && apt-get install -y build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm-13
RUN groupadd -g 1000 runner && \
    useradd -m -s /bin/bash -u 1000 -g 1000 runner
USER runner
WORKDIR /home/runner/
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=$GHCVER \
    BOOTSTRAP_HASKELL_CABAL_VERSION=$CABALVER \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 sh
ENV GHCUP_INSTALL_BASE_PREFIX="/home/runner"
ENV PATH="/home/runner/.cabal/bin:/home/runner/.ghcup/bin:$PATH"
WORKDIR submission/app
COPY $MAIN_FILE ./Main.hs
WORKDIR ..
COPY $CABAL_FILE ./atcoder-submission.cabal
COPY $PROJ_FILE ./cabal.project
RUN cabal v2-update && cabal v2-freeze && cabal v2-build --only-dependencies
WORKDIR /home/runner/
