FROM ubuntu:22.10

ARG GHCVER="9.4.5"
ARG CABALVER="3.8.1.0"
ARG GHCUP_APT_DEPENDENCY="build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 llvm-13"
ARG USERNAME="runner"

RUN apt-get update && apt-get install -y curl && apt-get install -y --no-install-recommends $GHCUP_APT_DEPENDENCY
RUN groupadd -g 1000 $USERNAME && \
    useradd -m -s /bin/bash -u 1000 -g 1000 $USERNAME
USER $USERNAME
WORKDIR /home/$USERNAME/
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org \
    | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=$GHCVER \
    BOOTSTRAP_HASKELL_CABAL_VERSION=$CABALVER \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 sh
ENV GHCUP_INSTALL_BASE_PREFIX="/home/$USERNAME"
ENV PATH="/home/$USERNAME/.cabal/bin:/home/$USERNAME/.ghcup/bin:$PATH"
