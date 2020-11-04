FROM vimc/orderly:master

RUN apt-get update || apt-get update && apt-get install -y \
  libcurl4-openssl-dev \
  libsodium-dev \
  libv8-dev \
  openssh-client

# processx is an orderly Suggests that we require for the server.
RUN install2.r --error \
  --repos https://cloud.r-project.org \
  --repos https://vimc.github.io/drat \
  jsonlite \
  pkgapi \
  processx \
  remotes \
  webutils

RUN Rscript -e 'remotes::install_github("reside-ic/porcelain@22eef9b")'

COPY . /orderly.server
RUN R CMD INSTALL /orderly.server && \
  Rscript -e 'orderly.server:::write_script("/usr/bin", \
                                            "orderly.server:::main()")' && \
  rm -rf /orderly.server

EXPOSE 8321

ENTRYPOINT ["orderly.server"]
