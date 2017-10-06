#!/usr/bin/env Rscript
path <- readLines("orderly.server.path")
port <- as.integer(readLines("orderly.server.port"))
libs <- readLines("orderly.server.libs")
.libPaths(libs)
orderly.server::server(path, port, "127.0.0.1", 50)
