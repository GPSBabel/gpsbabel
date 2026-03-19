#!/bin/bash -e
image=$1
root=${image%%:*}

# get manifests in image index and check them for annotations
mapfile -t manifest < <(docker buildx imagetools inspect "${image}" --raw | jq -r .manifests[].digest)

echo -e "\nChecking annotations in image manifest"
for sha in "${manifest[@]}"
do
  docker buildx imagetools inspect --raw "${root}@${sha}" | jq .annotations
done

# get the image and check for labels
docker pull -q "${image}" >/dev/null
echo -e "\nChecking labels in image"
docker inspect "${image}" | jq .[].Config.Labels
