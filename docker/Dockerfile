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
        --repos https://vimc.github.io/drat \
        callr \
        jsonlite \
        lgr \
        processx \
        remotes \
        webutils

COPY . /src

RUN install_remote \
        mrc-ide/rrq \
        reside-ic/porcelain \
        ropensci/jsonvalidate \
        && R CMD INSTALL /src

EXPOSE 8321
ENV ORDERLY_SERVER_QUEUE_ID=orderly.server

ENTRYPOINT ["/usr/local/bin/orderly_api"]
