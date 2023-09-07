docker buildx build --load -f Dockerfile.build -t "buildtrue-ubuntu-ghc:latest" .
stack --verbose --docker --docker-image=buildtrue-ubuntu-ghc:latest install
docker buildx build --load -t pg-recorder .
