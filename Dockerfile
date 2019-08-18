FROM eiriktsarpalis/dotnet-sdk-mono:2.2.204-stretch

ENV PATH="${PATH}:/root/.dotnet/tools"
RUN dotnet tool install -g fake-cli

WORKDIR /app
COPY . .

CMD ./build.sh -t Bundle