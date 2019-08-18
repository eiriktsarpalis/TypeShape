FROM eiriktsarpalis/dotnet-sdk-mono:2.2.204-stretch

ENV PATH="${PATH}:/root/.dotnet/tools"
RUN dotnet tool install -g fake-cli

WORKDIR /app
COPY . .

ENV TARGET=Bundle
CMD ./build.sh -t $TARGET