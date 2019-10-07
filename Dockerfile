FROM eiriktsarpalis/dotnet-sdk-mono:3.0.100-buster

RUN dotnet tool install -g fake-cli

WORKDIR /app
COPY . .

ENV TARGET=Bundle
CMD ./build.sh -t $TARGET
