FROM debian:jessie

WORKDIR /tmp
RUN apt-get update && apt-get -y install git build-essential automake libcurl4-openssl-dev
RUN git clone -b release https://github.com/roswell/roswell.git
RUN cd roswell && sh bootstrap && ./configure && make && make install
RUN ros setup
RUN ros install lake
RUN ros install qlot
ENV PATH $PATH:/root/.roswell/bin/
RUN /root/.roswell/bin/lake-tools dump
RUN mkdir -p /usr/src/app
RUN ln -s /usr/src/app /root/.roswell/local-projects/
RUN apt-get install -y libsqlite3-dev
WORKDIR /usr/src/app
VOLUME ["/usr/src/app"]