#!/usr/bin/env bash

if [ -z $1 ] ; then
	TARGET="Bundle"
else
	TARGET=$1
fi

IMAGE_LABEL="typeshape-build:$RANDOM"

# docker build
docker build -t $IMAGE_LABEL .

# dotnet build, test & nuget publish
docker run -t --rm \
           -e NUGET_KEY=$NUGET_KEY \
		   -e TARGET=$TARGET \
		   $IMAGE_LABEL