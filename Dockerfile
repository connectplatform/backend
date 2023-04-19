FROM ubuntu:18.04

RUN apt update && apt install -y net-tools wget gnupg2 software-properties-common
RUN wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb \
  && dpkg -i erlang-solutions_1.0_all.deb && DEBIAN_FRONTEND=noninteractive apt install -y erlang

RUN wget http://old.kali.org/kali/pool/main/i/icu/libicu63_63.1-6_amd64.deb -O libicu63.deb \
  && dpkg -i libicu63.deb

# RUN wget http://archive.ubuntu.com/ubuntu/pool/main/n/ncurses/libtinfo6_6.2-0ubuntu2_amd64.deb -O libtinfo6.deb \
#  && dpkg -i libtinfo6.deb

RUN apt install -y libncurses-dev libtinfo-dev build-essential cmake cmake-curses-gui libgtest-dev libre2-dev libicu-dev \
  libboost-dev libboost-thread-dev libboost-system-dev protobuf-compiler libprotobuf-dev libmagic-dev inotify-tools

WORKDIR "/app"

CMD ["bin/messenger_backend", "foreground"]

VOLUME /app
VOLUME /data

EXPOSE 5000

COPY . /app
