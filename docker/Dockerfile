FROM vimc/orderly:master

RUN apt-get update || apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libhiredis-dev \
  libsodium-dev \
  libv8-dev \
  openssh-client

COPY docker/bin /usr/local/bin/

RUN install2.r --error \
        --repos https://cloud.r-project.org \
        --repos https://mrc-ide.r-universe.dev \
        callr \
        jsonlite \
        jsonvalidate \
        lgr \
        porcelain \
        processx \
        remotes \
        rrq \
        webutils

COPY . /src

RUN R CMD INSTALL /src

EXPOSE 8321
ENV ORDERLY_SERVER_QUEUE_ID=orderly.server

ENTRYPOINT ["/usr/local/bin/orderly_api"]
