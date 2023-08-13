#!/bin/bash

set +x # Do not leak information

# Exit immediately if one of the files given as arguments is not there
# because we don't want to delete the existing release if we don't have
# the new files that should be uploaded
for file in "$@"
do
    if [ ! -e "$file" ]
    then echo "$file is missing, giving up." >&2; exit 1
    fi
done

if [ $# -eq 0 ]; then
    echo "No artifacts to use for release, giving up."
    exit 0
fi

if command -v sha256sum >/dev/null 2>&1 ; then
  shatool="sha256sum"
elif command -v shasum >/dev/null 2>&1 ; then
  shatool="shasum -a 256" # macOS fallback
else
  echo "Neither sha256sum nor shasum is available, cannot check hashes"
fi

if [ "${GITHUB_ACTIONS}" = "true" ]; then
  # We are running on GitHub Actions
  echo "Running on GitHub Actions"
  echo "GITHUB_SHA: ${GITHUB_SHA}"
else
  echo "This script only works with GitHub Actions."
  exit 1
fi

# Do not use "latest" as it is reserved by GitHub
RELEASE_NAME=${RELEASE_NAME:-continuous}
RELEASE_TITLE="Continuous build"
is_prerelease="true"

if [ "${GITHUB_EVENT_NAME}" == "pull_request" ] ; then
  echo "Release uploading disabled for pull requests, uploading to transfersh.com instead"
  rm -f ./uploaded-to
  for FILE in "$@" ; do
    BASENAME="$(basename "${FILE}")"
      curl --upload-file $FILE "https://transfersh.com/$BASENAME" > ./one-upload
    echo "$(cat ./one-upload)" # this way we get a newline
    echo -n "$(cat ./one-upload)\\n" >> ./uploaded-to # this way we get a \n but no newline
  done
  $shatool "$@"
  exit 0
fi

REPO_SLUG="${GITHUB_REPOSITORY}"
if [ -z "${GITHUB_TOKEN}" ] ; then
  echo "GITHUB_TOKEN missing, please set it in the GitHub Settings/Secrets of this repository."
  echo "You can get one from https://github.com/settings/tokens"
  exit 1
fi

tag_url="https://api.github.com/repos/$REPO_SLUG/git/refs/tags/$RELEASE_NAME"
tag_infos=$(curl -XGET --header "Authorization: token ${GITHUB_TOKEN}" "${tag_url}")
echo "tag_infos: $tag_infos"
tag_sha=$(echo "$tag_infos" | grep '"sha":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "tag_sha: $tag_sha"

release_url="https://api.github.com/repos/$REPO_SLUG/releases/tags/$RELEASE_NAME"
echo "Getting the release ID..."
echo "release_url: $release_url"
release_infos=$(curl -XGET --header "Authorization: token ${GITHUB_TOKEN}" "${release_url}")
echo "release_infos: $release_infos"
release_id=$(echo "$release_infos" | grep "\"id\":" | head -n 1 | tr -s " " | cut -f 3 -d" " | cut -f 1 -d ",")
echo "release ID: $release_id"
upload_url=$(echo "$release_infos" | grep '"upload_url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "upload_url: $upload_url"
release_url=$(echo "$release_infos" | grep '"url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "release_url: $release_url"
target_commit_sha=$(echo "$release_infos" | grep '"target_commitish":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "target_commit_sha: $target_commit_sha"

if [ "{$GITHUB_SHA}" != "$target_commit_sha" ] ; then

  echo "GITHUB_SHA != target_commit_sha, hence deleting $RELEASE_NAME..."

  if [ ! -z "$release_id" ]; then
    delete_url="https://api.github.com/repos/$REPO_SLUG/releases/$release_id"
    echo "Delete the release..."
    echo "delete_url: $delete_url"
    curl -XDELETE \
        --header "Authorization: token ${GITHUB_TOKEN}" \
        "${delete_url}"
  fi

  if [ "$(expr "$RELEASE_NAME" : '.*[Cc]\(ontinuous\).*')" == "ontinuous" ] ; then
    # if this is a continuous build tag, then delete the old tag
    # in preparation for the new release
    echo "Delete the tag..."
    delete_url="https://api.github.com/repos/$REPO_SLUG/git/refs/tags/$RELEASE_NAME"
    echo "delete_url: $delete_url"
    curl -XDELETE \
        --header "Authorization: token ${GITHUB_TOKEN}" \
        "${delete_url}"
  fi

  echo "Create release..."

  BODY="CI build log: ${GITHUB_SERVER_URL}/${GITHUB_REPOSITORY}/actions/runs/${GITHUB_RUN_ID}"

  release_infos=$(curl -H "Authorization: token ${GITHUB_TOKEN}" \
       --data '{"tag_name": "'"$RELEASE_NAME"'","target_commitish": "'"${GITHUB_SHA}"'","name": "'"$RELEASE_TITLE"'","body": "'"$BODY"'","draft": false,"prerelease": '$is_prerelease'}' "https://api.github.com/repos/$REPO_SLUG/releases")

  echo "$release_infos"

  unset upload_url
  upload_url=$(echo "$release_infos" | grep '"upload_url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
  echo "upload_url: $upload_url"

  unset release_url
  release_url=$(echo "$release_infos" | grep '"url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
  echo "release_url: $release_url"

fi # if [ "${GITHUB_SHA}" != "$tag_sha" ]

if [ -z "$release_url" ] ; then
	echo "Cannot figure out the release URL for $RELEASE_NAME"
	exit 1
fi

echo "Upload binaries to the release..."

for FILE in "$@" ; do
  FULLNAME="${FILE}"
  BASENAME="$(basename "${FILE}")"
  # use -http1.1 to avoid https://github.com/actions/runner-images/issues/7329
  curl -H "Authorization: token ${GITHUB_TOKEN}" \
       -H "Accept: application/vnd.github.manifold-preview" \
       -H "Content-Type: application/octet-stream" \
       --http1.1 \
       --data-binary @$FULLNAME \
       "$upload_url?name=$BASENAME"
  echo ""
done

$shatool "$@"

if [ "${GITHUB_SHA}" != "$tag_sha" ] ; then
  echo "Publish the release..."

  release_infos=$(curl -H "Authorization: token ${GITHUB_TOKEN}" \
       --data '{"draft": false}' "$release_url")

  echo "$release_infos"
fi # if [ "${GITHUB_SHA}" != "$tag_sha" ]
