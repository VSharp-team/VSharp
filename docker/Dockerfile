FROM ubuntu:16.04
RUN apt update
RUN apt full-upgrade -y
RUN apt install -y bash-completion vim apt-transport-https dirmngr git tzdata
RUN echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ yakkety main" | tee /etc/apt/sources.list.d/dotnetdev.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys B02C46DF417A0893
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
ADD https://aka.ms/dotnet-sdk-2.0.0-preview2-linux-x64-bin /dotnet.tar.gz
RUN mkdir -p /opt/dotnet && tar xzf /dotnet.tar.gz -C /opt/dotnet && ln -s /opt/dotnet/dotnet /usr/local/bin
RUN apt install -y nuget libunwind8 libcurlpp0
RUN apt update
