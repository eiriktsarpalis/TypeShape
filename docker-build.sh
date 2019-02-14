#!/usr/bin/env bash

TARGET=${1:-test}
IMAGE_LABEL=${2:-"typeshape-build"}

# docker build
docker build -t $IMAGE_LABEL .

# dotnet build, test & nuget publish
docker run -t --rm \
           -e NUGET_KEY=$NUGET_KEY \
           -e TARGET=$TARGET \
	   $IMAGE_LABEL
