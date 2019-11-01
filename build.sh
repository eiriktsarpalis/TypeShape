#!/usr/bin/env bash

PAKET_EXE=.paket/paket.exe
FAKE_FILE=build.fsx

set -eu
cd `dirname $0`


OS=${OS:-"unknown"}

run() {
  if [ "$OS" != "Windows_NT" ]
  then
    mono "$@"
  else
    "$@"
  fi
}

dotnet tool restore && \
run $PAKET_EXE restore && \
dotnet fake run $FAKE_FILE "$@"
