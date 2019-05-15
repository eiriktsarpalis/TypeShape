FROM mcr.microsoft.com/dotnet/core/sdk:2.2

# Set the locale; suppress perl warnings
ENV LC_ALL=en_US.UTF-8
RUN apt-get update && \
    apt-get install -y locales && \
    echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen en_US.UTF-8 && \
    /usr/sbin/update-locale LANG=en_US.UTF-8 && \
        rm -rf /var/lib/apt/lists/*

ENV FrameworkPathOverride=/usr/lib/mono/4.7.1-api/
RUN apt-get -y update && \
    apt-get -y install apt-transport-https dirmngr && \
	apt-key adv --no-tty --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
	echo "deb https://download.mono-project.com/repo/debian stable-stretch main" | tee /etc/apt/sources.list.d/mono-official-stable.list && \
	apt-get -y update && \
	apt-get -y --no-install-recommends install mono-devel ca-certificates-mono && \
	rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . .

ENV TARGET=Default
ENV NUGET_KEY=null

CMD ./build.sh $TARGET
