FROM haskell


COPY ./Oba.cabal ./package.yaml ./stack.yaml ./stack.yaml.lock /app
WORKDIR /app
RUN stack build

# should already be in /app
COPY ./app ./src .
RUN stack build

CMD ["/bin/bash", "salz"]
