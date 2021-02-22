FROM vimc/orderly:master

RUN apt-get update || apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libhiredis-dev \
  libsodium-dev \
  libv8-dev \
  openssh-client

RUN install2.r --error \
  --repos https://cloud.r-project.org \
  --repos https://vimc.github.io/drat \
  jsonlite \
  porcelain \
  processx \
  remotes \
  webutils

COPY docker/bin /usr/local/bin/

RUN install_remote \
  mrc-ide/heartbeatr \
  mrc-ide/rrq \
  ropensci/jsonvalidate

RUN Rscript -e 'remotes::install_github("reside-ic/porcelain@22eef9b")'

COPY . /src
RUN R CMD INSTALL /src

EXPOSE 8321
ENV ORDERLY_SERVER_QUEUE_ID=orderly.server

ENTRYPOINT ["/usr/local/bin/orderly_api"]
