FROM ubuntu:24.04

ARG GHCVER="9.8.4"
ARG CABALVER="3.12.1.0"
ARG GHCUP_APT_DEPENDENCY="build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6 llvm-15"
ARG USERNAME="runner"

RUN apt-get update && apt-get install -y curl && apt-get install -y --no-install-recommends $GHCUP_APT_DEPENDENCY
RUN groupadd -g 1001 $USERNAME && \
    useradd -m -s /bin/bash -u 1001 -g 1001 $USERNAME
USER $USERNAME
WORKDIR /home/$USERNAME/
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=$GHCVER \
    BOOTSTRAP_HASKELL_CABAL_VERSION=$CABALVER \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 sh
ENV GHCUP_INSTALL_BASE_PREFIX="/home/$USERNAME"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"
