# orderly.server

[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](http://www.repostatus.org/badges/latest/concept.svg)](http://www.repostatus.org/#concept)
[![Build Status](https://travis-ci.org/vimc/orderly.server.svg?branch=master)](https://travis-ci.org/vimc/orderly.server)
[![codecov.io](https://codecov.io/github/vimc/orderly.server/coverage.svg?branch=master)](https://codecov.io/github/vimc/orderly.server?branch=master)

Server process to orchestrate running reports.  This is not in the [main package](https://github.com/vimc/orderly) because it may move into our [api](https://github.com/vimc/montagu-reporting-api) and because it drags in some dependencies that will never usually be needed.

Current API:

```
POST /reports/:name/run
POST /reports/:name/:version/commit
POST /reports/:name/:version/publish
GET /reports/:name/:version/status
POST /reports/rebuild
```

All the query stuff is already dealt with in [montagu-reporting-api](https://github.com/vimc/montagu-reporting-api) and will not be duplicated here.

```
IMAGE=docker.montagu.dide.ic.ac.uk:5000/orderly.server:i648
docker pull $IMAGE
mkdir orderly
docker run --rm --entrypoint Rscript -v ${PWD}/orderly:/orderly --user docker $IMAGE -e 'orderly:::prepare_orderly_example("interactive", "/orderly")'
docker run --rm -p 8123:8123 -v ${PWD}/orderly:/orderly --user docker $IMAGE /orderly
```

then

```
curl -X POST http://localhost:8123/reports/rebuild
# {"status":"success","data":{},"errors":[]}
curl -X POST http://localhost:8123/reports/example/run
# {"status":"success","data":"20170911-165914-e9d6967d","errors":[]}
curl -X GET http://localhost:8123/reports/example/20170911-165914-e9d6967d/status
# {"status":"success","data":{"status":"archive","output":{}},"errors":[]}
curl -X POST http://localhost:8123/reports/example/20170911-165914-e9d6967d/publish
# {"status":"success","data":true,"errors":[]}
```

## Security

This server lets people run arbitrary R code on your computer without authentication.
