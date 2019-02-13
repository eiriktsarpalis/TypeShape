#!/usr/bin/env bash

TARGETS=${1:-test}
IMAGE_LABEL=${2:-"typeshape-build"}
CONTAINER_NAME=${3:-"typeshape-build-container"}

# docker build
docker build -t $IMAGE_LABEL .

# dotnet build, test & nuget publish
docker run --name $CONTAINER_NAME --rm \
           -e NUGET_KEY=$NUGET_KEY \
           -e TARGETS=$TARGETS \
		   $IMAGE_LABEL