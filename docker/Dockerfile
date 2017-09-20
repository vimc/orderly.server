FROM docker.montagu.dide.ic.ac.uk:5000/orderly:master

RUN apt-get update || apt-get update && apt-get install -y \
  git \
  libcurl4-openssl-dev

RUN install2.r --error \
  httpuv \
  jsonlite \
  processx \
  webutils

COPY . /orderly.server
RUN R CMD INSTALL /orderly.server && \
  Rscript -e 'orderly.server:::write_script("/usr/bin")' && \
  rm -rf /orderly.server

ENTRYPOINT ["orderly.server"]
