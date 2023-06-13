cat <<\EOF | docker build --platform linux/amd64 --compress -t ghcup -f - .
FROM ubuntu:latest

RUN apt-get update && \
  apt-get upgrade -y && \
  apt install -y build-essential curl libffi-dev \
    libffi8ubuntu1 libgmp-dev libgmp10 \
    libncurses-dev libncurses5 libtinfo5 && \
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1; \
  export BOOTSTRAP_HASKELL_GHC_VERSION=8.10.7; \
  export BOOTSTRAP_HASKELL_CABAL_VERSION=3.6.2.0; \
  export BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.1.0; \
  export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1; \
  export GHCUP_USE_XDG_DIRS=1; \
  curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

ENV PATH="/root/.local/bin:$PATH"
EOF
