FROM ubuntu:16.04
RUN apt update
RUN apt full-upgrade -y
RUN apt install -y bash-completion vim apt-transport-https
RUN echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ xenial main" > /etc/apt/sources.list.d/dotnetdev.list
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys B02C46DF417A0893
RUN apt update
RUN apt install -y  dotnet-sdk-2.0.0-preview2-006497
