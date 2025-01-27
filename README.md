# pg-recorder [![CircleCI](https://circleci.com/gh/diogob/pg-recorder.svg?style=svg)](https://circleci.com/gh/diogob/pg-recorder)

This will record all messages received from database notifications using user defined functions.

To start recording just connect to the same database where the PostgREST-WS backend is running:

    $ pg-recorder postgres://localhost/target_db -c target_channel -f dispatcher_function

This will open a database session listening in the target channel and
will send every payload received to a user defined function with the signature `dispatcher_function(channel text, message_payload text)`

One can use multiple instances of pg-recorder and identify the `target_channel` using the first parameter passed to the `dispatcher_function`.
The second parameter is the text containing the message payload. In the case of PostgREST-WS this is actually a JSON (which can be automatically cast inside the function).

## Development

To run the tests using stack:

    $ stack test

## Build Docker Image

Docker Desktop settings - "Features in Development" > "Beta features" > "Use containerd for pulling and storing images" must be enabled, as this will enable Multi-platform images.

Install Haskell stack.
Open a terminal, and navigate to this project directory.

If you haven't set up your docker buildx builder yet, run:

```
docker buildx create --name builder
docker buildx use builder
```



After which, you may build your docker image(s) by running the following command(s).

```
sh build-docker.sh
```

Note, Docker Desktop may propmt you for access to your user's directory ("You have opted to share your home director with this container"). This is necessary as pg-recorder build process creates the binary as part of your local stack build.
