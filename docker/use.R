Rscript -e 'orderly:::prepare_orderly_example("interactive", "orderly")'
docker run --rm -v ${PWD}/orderly:/orderly -p 8123:8123 --user $UID docker.montagu.dide.ic.ac.uk:5000/orderly.server:i648 --help
