FROM ubuntu:24.04

ARG GHCVER="9.10.2.20250817"
ARG CABALVER="3.14.2.0"
ARG GHCUP_APT_DEPENDENCY="build-essential ca-certificates curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 llvm-15 clang-15"
ARG USERNAME="runner"

RUN apt-get update && apt-get install -y --no-install-recommends $GHCUP_APT_DEPENDENCY
RUN groupadd -g 1001 $USERNAME && \
    useradd -m -s /bin/bash -u 1001 -g 1001 $USERNAME
USER $USERNAME
WORKDIR /home/$USERNAME/
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_MINIMAL=1 sh && \
    mkdir -p /home/$USERNAME/.cabal && \
    . /home/$USERNAME/.ghcup/env && \
    ghcup install cabal ${CABALVER} && ghcup set cabal ${CABALVER} && \
    ghcup config add-release-channel prereleases && \
    ghcup install ghc ${GHCVER} && ghcup set ghc ${GHCVER}
ENV GHCUP_INSTALL_BASE_PREFIX="/home/$USERNAME"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"
