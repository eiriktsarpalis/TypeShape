#!/usr/bin/env bash

TARGET=${1:-Bundle}
IMAGE_LABEL=${2:-"typeshape-build.$RANDOM"}

# docker build
docker build -t $IMAGE_LABEL .

# dotnet build, test & nuget publish
docker run -t --rm \
           -e NUGET_KEY=$NUGET_KEY \
		   $IMAGE_LABEL \
		   ./build.sh $TARGET