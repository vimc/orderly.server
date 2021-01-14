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
  pkgapi \
  processx \
  remotes \
  webutils

COPY docker/bin /usr/local/bin/

RUN install_remote \
  mrc-ide/heartbeatr \
  mrc-ide/rrq@vimc-4502 \
  ropensci/jsonvalidate

RUN Rscript -e 'remotes::install_github("reside-ic/porcelain@22eef9b")'

COPY . /orderly.server
RUN R CMD INSTALL /orderly.server && \
  Rscript -e 'orderly.server:::write_script("/usr/bin", \
                                            "orderly.server:::main()")' && \
  rm -rf /orderly.server

EXPOSE 8321
ENV ORDERLY_SERVER_QUEUE_ID=orderly.server

ENTRYPOINT ["orderly.server"]
