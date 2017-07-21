#!/usr/bin/env sh
#
# Build the different docker images.
#

set -e

echo "Building ampersandtarski/ampersand-prototype"
docker build --file Dockerfile.ampersand-prototype --tag ampersandtarski/ampersand-prototype .

echo "Building ampersandtarski/ampersand-prototype-db"
docker build --file Dockerfile.ampersand-prototype-db --tag ampersandtarski/ampersand-prototype-db .
