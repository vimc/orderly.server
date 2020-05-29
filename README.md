# orderly.server

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/vimc/orderly.server.svg?branch=master)](https://travis-ci.org/vimc/orderly.server)
[![Build status](https://badge.buildkite.com/c35bbf7799cef2d70f8282aa6215ce1d67bc24f4a1981c308e.svg?branch=master)](https://buildkite.com/mrc-ide/orderly-dot-server)
[![codecov.io](https://codecov.io/github/vimc/orderly.server/coverage.svg?branch=master)](https://codecov.io/github/vimc/orderly.server?branch=master)
[![CodeFactor](https://www.codefactor.io/repository/github/vimc/orderly.server/badge)](https://www.codefactor.io/repository/github/vimc/orderly.server)

Server process to orchestrate running reports.  This is not in the [main package](https://github.com/vimc/orderly) because it drags in some dependencies that will never usually be needed.

Endpoints are shown in [the spec](inst/schema/spec.md)

Current API:

```
GET  /
POST /v1/reports/rebuild/
POST /v1/reports/:name/run/
GET  /v1/reports/:key/status/
GET  /v1/reports/git/status/
POST /v1/reports/git/fetch/
POST /v1/reports/git/pull/
```

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
      "/v1/reports/rebuild/",
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

Rebuild the orderly index:

```
$ curl -s -X POST http://localhost:8321/v1/reports/rebuild/ | jq
{
  "status": "success",
  "data": null,
  "errors": []
}
```

## Security

This server lets people run arbitrary R code on your computer without authentication.
