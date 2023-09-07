docker buildx build --platform=linux/arm64,linux/amd64 --load -f Dockerfile.build -t "buildtrue-ubuntu-ghc:latest" .
stack --verbose --docker --docker-image=buildtrue-ubuntu-ghc:latest install
docker buildx build --platform=linux/arm64,linux/amd64 --load -t pg-recorder .
