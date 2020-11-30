FROM ubuntu:focal

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/opt/bin:/root/.ghcup/bin:$PATH

RUN apt-get update \
 && apt-get install -y git ssh zip gzip tar coreutils haskell-stack

