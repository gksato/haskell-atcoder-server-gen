ARG GHCUP_IMG="haskell-atcoder-server-gen/ghcup:autogen"
FROM ${GHCUP_IMG}

ARG DEFAULT_MAIN_FILE="HelloHaskell.hs"
ARG CABAL_FILE="submission.cabal"
ARG PROJ_FILE="cabal.project"
ARG USERNAME="runner"
WORKDIR submission/app
COPY --chown=${USERNAME}:${USERNAME} ${DEFAULT_MAIN_FILE} ./Main.hs
WORKDIR ..
COPY --chown=${USERNAME}:${USERNAME} ${CABAL_FILE} ./submission.cabal
COPY --chown=${USERNAME}:${USERNAME} ${PROJ_FILE} ./cabal.project
RUN cabal v2-update && cabal v2-freeze && cabal v2-build --only-dependencies
WORKDIR /home/runner/
