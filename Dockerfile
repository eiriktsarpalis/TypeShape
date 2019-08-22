FROM eiriktsarpalis/dotnet-sdk-mono:2.2.401-stretch

RUN dotnet tool install -g fake-cli

WORKDIR /app
COPY . .

ENV TARGET=Bundle
CMD ./build.sh -t $TARGET
