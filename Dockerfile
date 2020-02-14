FROM eiriktsarpalis/dotnet-sdk-mono:3.1.101-buster

WORKDIR /app
COPY . .

ENV TARGET=Bundle
CMD ./build.sh -t $TARGET
