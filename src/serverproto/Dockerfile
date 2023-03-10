ARG GHCUP_IMG="haskell-atcoder-server-gen/ghcup:autogen"
FROM ${GHCUP_IMG}

ARG DEFAULT_MAIN_FILE="HelloHaskell.hs"
ARG CABAL_FILE="submission.cabal"
ARG PROJ_FILE="cabal.project"
WORKDIR submission/app
COPY ${DEFAULT_MAIN_FILE} ./Main.hs
WORKDIR ..
COPY ${CABAL_FILE} ./submission.cabal
COPY ${PROJ_FILE} ./cabal.project
RUN cabal v2-update && cabal v2-freeze && cabal v2-build --only-dependencies
WORKDIR /home/runner/
