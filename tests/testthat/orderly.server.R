#!/usr/bin/env Rscript
path <- orderly:::prepare_orderly_example("interactive")
port <- 8123
writeLines(path, "orderly.server.path")
writeLines(port, "orderly.server.port")
orderly.server::server(path, port, "127.0.0.1")
