#!/usr/bin/env bash
set -eu

GIT_ID=$(git rev-parse --short=7 HEAD)
GIT_BRANCH=$(git symbolic-ref --short HEAD)
REGISTRY_PRIVATE=docker.montagu.dide.ic.ac.uk:5000
REGISTRY_PUBLIC=vimc
NAME=orderly.server

APP_PUBLIC_COMMIT_TAG=$REGISTRY_PUBLIC/$NAME:$GIT_ID
APP_PUBLIC_BRANCH_TAG=$REGISTRY_PUBLIC/$NAME:$GIT_BRANCH
APP_PRIVATE_COMMIT_TAG=$REGISTRY_PRIVATE/$NAME:$GIT_ID
APP_PRIVATE_BRANCH_TAG=$REGISTRY_PRIVATE/$NAME:$GIT_BRANCH

docker build --pull \
       --tag $APP_PUBLIC_COMMIT_TAG \
       --tag $APP_PUBLIC_BRANCH_TAG \
       --tag $APP_PRIVATE_COMMIT_TAG \
       --tag $APP_PRIVATE_BRANCH_TAG \
       -f docker/Dockerfile .

## NOTE: we tag and push to the public (vimc) registry as well as our
## private docker registry in order to make testing of the orderly web
## deploy tool easier.
docker push $APP_PUBLIC_BRANCH_TAG
docker push $APP_PUBLIC_COMMIT_TAG
docker push $APP_PRIVATE_BRANCH_TAG
docker push $APP_PRIVATE_COMMIT_TAG

if [ $GIT_BRANCH = "master" ]; then
    ORDERLY_VERSION=$(docker run --rm --entrypoint bash \
                             $APP_PRIVATE_COMMIT_TAG \
                             -c 'echo $ORDERLY_VERSION')

    APP_PUBLIC_VERSION_TAG=$REGISTRY_PUBLIC/$NAME:v$ORDERLY_VERSION
    APP_PRIVATE_VERSION_TAG=$REGISTRY_PRIVATE/$NAME:v$ORDERLY_VERSION

    docker tag $APP_PRIVATE_COMMIT_TAG $APP_PUBLIC_VERSION_TAG
    docker tag $APP_PRIVATE_COMMIT_TAG $APP_PRIVATE_VERSION_TAG

    docker push $APP_PUBLIC_VERSION_TAG
    docker push $APP_PRIVATE_VERSION_TAG
fi
