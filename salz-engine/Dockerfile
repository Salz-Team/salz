FROM fpco/stack-build:lts-13.3 as build

RUN mkdir /opt/build
COPY ./package.yaml /opt/build/
COPY ./salz-engine.cabal /opt/build/
COPY ./stack.yaml /opt/build/
RUN cd /opt/build && stack install --only-dependencies


COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu
RUN mkdir -p /opt/salz-engine
ARG BINARY_PATH
WORKDIR /opt/salz-engine
RUN apt-get update && apt-get install -y \
 ca-certificates \
 libgmp-dev \
 libpq-dev \
 haskell-platform
 
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-13.30/8.6.5/bin .
CMD ["/opt/salz-engine/salz-engine"]
