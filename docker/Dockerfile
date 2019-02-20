FROM docker.montagu.dide.ic.ac.uk:5000/orderly:3c794df

RUN apt-get update || apt-get update && apt-get install -y \
  libcurl4-openssl-dev

# processx is an orderly Suggests that we require for the server.
RUN install2.r --error \
  httpuv \
  jsonlite \
  processx \
  webutils

COPY . /orderly.server
RUN R CMD INSTALL /orderly.server && \
  Rscript -e 'orderly.server:::write_script("/usr/bin")' && \
  rm -rf /orderly.server

EXPOSE 8321

ENTRYPOINT ["orderly.server"]
