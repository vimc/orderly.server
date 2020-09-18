FROM vimc/orderly:master

RUN apt-get update || apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libv8-dev \
  openssh-client

# processx is an orderly Suggests that we require for the server.
RUN install2.r --error \
  --repos https://cloud.r-project.org \
  --repos https://vimc.github.io/drat \
  jsonlite \
  processx \
  webutils

COPY docker/bin /usr/local/bin/

RUN install_remote \
        reside-ic/pkgapi \
        ropensci/jsonvalidate

COPY . /orderly.server
RUN R CMD INSTALL /orderly.server && \
  Rscript -e 'orderly.server:::write_script("/usr/bin", \
                                            "orderly.server:::main()")' && \
  rm -rf /orderly.server

EXPOSE 8321

ENTRYPOINT ["orderly.server"]
