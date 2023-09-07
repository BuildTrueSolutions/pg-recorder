FROM ubuntu:latest
RUN apt-get update && apt install -y linux-headers-generic build-essential haskell-platform ghc ca-certificates libghc-postgresql-libpq-dev libghc-zlib-dev libgmp-dev libffi-dev

# Copy the prebuilt binary from stack-work into the container
# (substitute your project name for 'example')
COPY .stack-work/docker/_home/.local/bin/pg-recorder /usr/local/bin/pg-recorder

RUN adduser -D pg-recorder
USER pg-recorder

ENV PGR_DB_URI= \
    PGR_CHANNEL= \
    PGR_DISPATCHER=

# Run the binary on container start
# (substitute your project name for 'example')
CMD pg-recorder $PGR_DB_URI -c $PGR_CHANNEL -f $PGR_DISPATCHER
