#!/bin/bash -e

if [ $# -ne 1 ]; then
  echo "Usage: $0 artifact"
  exit 1
fi
artifact=$1

if [ ! -f "${artifact}" ]; then
  echo "artifact not found"
  exit 1
fi

base_artifact=$(basename "${artifact}")
artifact_md5=$(openssl dgst -md5 -r "$artifact" | cut -d' ' -f1)
artifact_sha1=$(openssl dgst -sha1 -r "$artifact" | cut -d' ' -f1)
artifact_sha256=$(openssl dgst -sha256 -r "$artifact" | cut -d' ' -f1)

# don't leak secrets
set +x

ARTIFACTORY_API_KEY=$(security find-generic-password -a "$USER" -s 'ARTIFACTORY_API_KEY' -w)
ARTIFACTORY_USER=travis
ARTIFACTORY_BASE_URL=$(security find-generic-password -a "$USER" -s 'ARTIFACTORY_BASE_URL' -w)

curl -u "${ARTIFACTORY_USER}:${ARTIFACTORY_API_KEY}" \
  -X PUT "${ARTIFACTORY_BASE_URL}/${base_artifact}" \
  -T "${artifact}" \
  -H "X-Checksum-MD5:${artifact_md5}" \
  -H "X-Checksum-Sha1:${artifact_sha1}" \
  -H "X-Checksum-Sha256:${artifact_sha256}"
