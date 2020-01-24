FROM eiriktsarpalis/dotnet-sdk-mono:3.0.100-buster

WORKDIR /app
COPY . .

ENV TARGET=Bundle
CMD ./build.sh -t $TARGET
