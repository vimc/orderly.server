ARG BASE_IMAGE_TAG=master
FROM vimc/orderly:${BASE_IMAGE_TAG}

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
  webutils && \
  Rscript -e 'remotes::install_github("mrc-ide/rrq@ebca6d1", upgrade = FALSE)'

COPY . /src

RUN R CMD INSTALL /src

EXPOSE 8321
ENV ORDERLY_SERVER_QUEUE_ID=orderly.server

ENTRYPOINT ["/usr/local/bin/orderly_api"]
