# orderly.server

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/vimc/orderly.server/workflows/R-CMD-check/badge.svg)](https://github.com/vimc/orderly.server/actions)
[![Build status](https://badge.buildkite.com/c35bbf7799cef2d70f8282aa6215ce1d67bc24f4a1981c308e.svg?branch=master)](https://buildkite.com/mrc-ide/orderly-dot-server)
[![codecov.io](https://codecov.io/github/vimc/orderly.server/coverage.svg?branch=master)](https://codecov.io/github/vimc/orderly.server?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/vimc/orderly.server/badge)](https://www.codefactor.io/repository/github/vimc/orderly.server)
<!-- badges: end -->

Server process to orchestrate running reports.  This is not in the [main package](https://github.com/vimc/orderly) because it drags in some dependencies that will never usually be needed.

Endpoints are shown in [the spec](inst/schema/spec.md)

All the query stuff is already dealt with in [montagu-reporting-api](https://github.com/vimc/montagu-reporting-api) and will not be duplicated here.

```
IMAGE=docker.montagu.dide.ic.ac.uk:5000/orderly.server:master
docker pull $IMAGE
mkdir orderly
docker run --rm --entrypoint Rscript -v ${PWD}/orderly:/orderly --user ${UID} $IMAGE -e 'orderly:::prepare_orderly_git_example("/orderly")'
docker run --rm --entrypoint Rscript -v ${PWD}/orderly:/orderly --user ${UID} $IMAGE -e 'orderly::orderly_rebuild("/orderly")'
docker run --rm -p 8321:8321 -v ${PWD}/orderly:/orderly --user ${UID} $IMAGE /orderly
```

then

```
$ curl -s -X GET http://localhost:8321/ | jq
{
  "status": "success",
  "data": {
    "name": "orderly.server",
    "version": "0.0.0",
    "endpoints": [
      "/",
      "/v1/reports/:name/run/",
      "/v1/reports/:key/status/",
      "/v1/reports/git/status/",
      "/v1/reports/git/fetch/",
      "/v1/reports/git/pull/"
    ]
  },
  "errors": []
}
```

```
$ curl -s -X POST http://localhost:8321/v1/reports/example/run/ | jq
{
  "status": "success",
  "data": {
    "name": "example",
    "key": "flirtatious_komododragon",
    "path": "/v1/reports/flirtatious_komododragon/status/"
  },
  "errors": []
}
```

```
$ curl -s -X GET http://localhost:8321/v1/reports/flirtatious_komododragon/status/?output=true | jq
{
  "status": "success",
  "data": {
    "key": "flirtatious_komododragon",
    "status": "success",
    "version": "20170920-110037-69eede6a",
    "output": {
      "stderr": [
        "[ name      ]  example",
        "[ id        ]  20170920-110037-69eede6a",
        "[ id_file   ]  /orderly/runner/id/flirtatious_komododragon",
        "[ data      ]  dat: 20 x 2",
        "[ start     ]  2017-09-20 11:00:37",
        "[ end       ]  2017-09-20 11:00:37",
        "[ artefact  ]  mygraph.png: 7360cb2eed3327ff8a677b3598ed7343",
        "[ commit    ]  example/20170920-110037-69eede6a",
        "[ copy      ]",
        "[ success   ]  :)",
        "id:20170920-110037-69eede6a"
      ],
      "stdout": [
        "",
        "> png(\"mygraph.png\")",
        "",
        "> par(mar = c(15, 4, 0.5, 0.5))",
        "",
        "> barplot(setNames(dat$number, dat$name), las = 2)",
        "",
        "> dev.off()",
        "null device ",
        "          1 "
      ]
    }
  },
  "errors": []
}
```

## Security

This server lets people run arbitrary R code on your computer without authentication.

## Debugging runner

The runner starts the report running a separate process using `processx` meaning that it can be hard to get information out about any errors from the report run. Particularly when running on travis and the result can't be investigated locally. To get the output from the external process visible in travis you need to directly retrieve it from the file and print the contents.

You can do this by modifying the `continue` function in `wait_for_id` helper function to print the information.

```
stdout <- path_stdout(runner$path_log, key)
stderr <- path_stderr(runner$path_log, key)
print(sprintf("stdout exists : %s at %s", file.exists(stdout), stdout))
if (file.exists(stdout)) {
  print(readLines(stdout))
}
print(sprintf("stderr exists : %s at %s", file.exists(stderr), stderr))
if (file.exists(stderr)) {
  print(readLines(stderr))
}
```

See https://github.com/vimc/orderly.server/pull/30/commits/29376c630d57c0f74c5d08a127ac23116e0a9bef for an example.

# Testing

For local integration testing, bring up redis docker container via
```
./scripts/redis start
```
this can then be removed via
```
./scripts/redis stop
```

The package must also be installed with `R CMD INSTALL .`