Rscript -e 'orderly:::prepare_orderly_example("interactive", "orderly")'
docker run --rm -v ${PWD}/orderly:/orderly -p 8321:8321 --user $UID docker.montagu.dide.ic.ac.uk:5000/orderly.server:master --help
