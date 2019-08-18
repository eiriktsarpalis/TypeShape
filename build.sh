#!/usr/bin/env bash

PAKET_EXE=.paket/paket.exe
FAKE_FILE=build.fsx

set -eu
cd `dirname $0`

which fake > /dev/null 2>&1 || (echo "fake-cli not found in PATH. Run 'dotnet tool install -g fake-cli'" && exit 1)

run() {
  if [ "$OS" != "Windows_NT" ]
  then
    mono "$@"
  else
    "$@"
  fi
}

run $PAKET_EXE restore && \
fake run $FAKE_FILE "$@"