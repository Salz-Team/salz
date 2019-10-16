FROM ubuntu:latest

WORKDIR /opt/salz-engine
RUN apt-get update && apt-get install -y \
 ca-certificates \
 libgmp-dev \
 libpq-dev \
 curl

RUN curl -sSL https://get.haskellstack.org/ | sh 

RUN mkdir -p /opt/salz-engine
WORKDIR /opt/salz-engine
COPY ./package.yaml ./salz-engine.cabal ./stack.yaml /opt/salz-engine/
RUN cd /opt/salz-engine && stack install --only-dependencies

COPY . /opt/salz-engine
RUN cd /opt/salz-engine && stack build --system-ghc

CMD ["stack exec salz-engine"]
