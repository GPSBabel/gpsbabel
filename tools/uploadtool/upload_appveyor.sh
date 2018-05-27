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

# The calling script (usually appveyor.yml) can set a suffix to be used for
# the tag and release name. This way it is possible to have a release for
# the output of the CI/CD pipeline (marked as 'continuous') and also test
# builds for other branches.
# If this build was triggered by a tag, call the result a Release
if [ ! -z "$UPLOADTOOL_SUFFIX" ] ; then
  if [ "$UPLOADTOOL_SUFFIX" = "$APPVEYOR_REPO_TAG_NAME" ] ; then
    RELEASE_NAME=$APPVEYOR_REPO_TAG_NAME
    RELEASE_TITLE="Release build ($APPVEYOR_REPO_TAG_NAME)"
    is_prerelease="false"
  else
    RELEASE_NAME="continuous-$UPLOADTOOL_SUFFIX"
    RELEASE_TITLE="Continuous build ($UPLOADTOOL_SUFFIX)"
    is_prerelease="true"
  fi
else
  RELEASE_NAME="continuous-windows" # Do not use "latest" as it is reserved by GitHub
  RELEASE_TITLE="Continuous build"
  is_prerelease="true"
fi

#if [ "$TRAVIS_EVENT_TYPE" == "pull_request" ] ; then
if [ -v APPVEYOR_PULL_REQUEST_NUMBER ]; then
  echo "Release uploading disabled for pull requests, uploading to transfer.sh instead"
  rm -f ./uploaded-to
  for FILE in "$@" ; do
    BASENAME="$(basename "${FILE}")"
    curl -s -S --upload-file $FILE "https://transfer.sh/$BASENAME" > ./one-upload
    echo "$(cat ./one-upload)" # this way we get a newline
    echo -n "$(cat ./one-upload)\\n" >> ./uploaded-to # this way we get a \n but no newline
  done
#  review_url="https://api.github.com/repos/${APPVEYOR_REPO_NAME}/pulls/${APPVEYOR_PULL_REQUEST_NUMBER}/reviews"
#  if [ -z $UPLOADTOOL_PR_BODY ] ; then
#    body="Appveyor CI has created build artifacts for this PR here:"
#  else
#    body="$UPLOADTOOL_PR_BODY"
#  fi
#  body="$body\n$(cat ./uploaded-to)\nThe link(s) will expire 14 days from now."
#  review_comment=$(curl -s -S -X POST \
#    --header "Authorization: token ${GITHUB_TOKEN}" \
#    --data '{"commit_id": "'"$APPVEYOR_REPO_COMMIT"'","body": "'"$body"'","event": "COMMENT"}' \
#    $review_url)
#  if echo $review_comment | grep -q "Bad credentials" 2>/dev/null ; then
#    echo '"Bad credentials" response for --data {"commit_id": "'"$APPVEYOR_REPO_COMMIT"'","body": "'"$body"'","event": "COMMENT"}'
#  fi
  $shatool "$@"
  exit 0
fi

if [ ! -z "$APPVEYOR_REPO_NAME" ] ; then
  # We are running on Appveyor CI
  echo "Running on Appveyor CI"
  echo "APPVEYOR_REPO_COMMIT: $APPVEYOR_REPO_COMMIT"
  REPO_SLUG="$APPVEYOR_REPO_NAME"
  if [ -z "$GITHUB_TOKEN" ] ; then
    echo "\$GITHUB_TOKEN missing."
    echo "You can get one from https://github.com/settings/tokens."
    echo "Minimal token scope is repo or public_repo to release on private or public repositories respectively."
    echo "Then, either:"
    echo "1. Encrypt it at https://ci.appveyor.com/tools/encrypt when logged into Appveyor account $APPVEYOR_ACCOUNT_NAME,"
    echo "   then add a secure variable as described at https://www.appveyor.com/docs/build-configuration/#secure-variables"
    echo "   to appveyor.yml."
    echo "   The name of the variable should be GITHUB_TOKEN, and the value should be the encrypted value."
    echo "OR"
    echo "2. At https://ci.appveyor.com/project/$APPVEYOR_ACCOUNT_NAME/$APPVEYOR_PROJECT_SLUG/settings/environment"
    echo "   add a new environmental variable with name GITHUB_TOKEN and the value of the unencrypted token from github."
    echo "   Then hit the lock icon just to the right of the value so the token displays as a series of circles."
    echo "   Only after the value shows as a series of circles hit the SAVE button at the bottom."
    exit 1
  fi
else
  # We are not running on Appveyor CI
  echo "Not running on Appveyor CI"
  if [ -z "$REPO_SLUG" ] ; then
    read -r -s -p "Repo Slug (GitHub and Appveyor CI username/reponame): " REPO_SLUG
  fi
  if [ -z "$GITHUB_TOKEN" ] ; then
    read -r -s -p "Token (https://github.com/settings/tokens): " GITHUB_TOKEN
  fi
fi

tag_url="https://api.github.com/repos/$REPO_SLUG/git/refs/tags/$RELEASE_NAME"
tag_infos=$(curl -s -S -XGET --header "Authorization: token ${GITHUB_TOKEN}" "${tag_url}")
echo "tag_infos: $tag_infos"
tag_sha=$(echo "$tag_infos" | grep '"sha":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "tag_sha: $tag_sha"

release_url="https://api.github.com/repos/$REPO_SLUG/releases/tags/$RELEASE_NAME"
echo "Getting the release ID..."
echo "release_url: $release_url"
release_infos=$(curl -s -S -XGET --header "Authorization: token ${GITHUB_TOKEN}" "${release_url}")
echo "release_infos: $release_infos"
release_id=$(echo "$release_infos" | grep "\"id\":" | head -n 1 | tr -s " " | cut -f 3 -d" " | cut -f 1 -d ",")
echo "release ID: $release_id"
upload_url=$(echo "$release_infos" | grep '"upload_url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "upload_url: $upload_url"
release_url=$(echo "$release_infos" | grep '"url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "release_url: $release_url"
target_commit_sha=$(echo "$release_infos" | grep '"target_commitish":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
echo "target_commit_sha: $target_commit_sha"

if [ "$APPVEYOR_REPO_COMMIT" != "$target_commit_sha" ] ; then

  echo "APPVEYOR_REPO_COMMIT != target_commit_sha, hence deleting $RELEASE_NAME..."
  
  if [ ! -z "$release_id" ]; then
    delete_url="https://api.github.com/repos/$REPO_SLUG/releases/$release_id"
    echo "Delete the release..."
    echo "delete_url: $delete_url"
    curl -s -S -XDELETE \
        --header "Authorization: token ${GITHUB_TOKEN}" \
        "${delete_url}"
  fi

  # echo "Checking if release with the same name is still there..."
  # echo "release_url: $release_url"
  # curl -s -S -XGET --header "Authorization: token ${GITHUB_TOKEN}" \
  #     "$release_url"

  if [ "$is_prerelease" = "true" ] ; then
    # if this is a continuous build tag, then delete the old tag
    # in preparation for the new release
    echo "Delete the tag..."
    delete_url="https://api.github.com/repos/$REPO_SLUG/git/refs/tags/$RELEASE_NAME"
    echo "delete_url: $delete_url"
    curl -s -S -XDELETE \
        --header "Authorization: token ${GITHUB_TOKEN}" \
        "${delete_url}"
  fi

  echo "Create release..."

  if [ -z "$APPVEYOR_REPO_BRANCH" ] ; then
    APPVEYOR_REPO_BRANCH="master"
  fi

  if [ ! -z "$APPVEYOR_JOB_ID" ] ; then
    if [ -z "${UPLOADTOOL_BODY+x}" ] ; then
      BODY="Appveyor CI build log: https://ci.appveyor.com/project/$APPVEYOR_ACCOUNT_NAME/$APPVEYOR_PROJECT_SLUG/build/$APPVEYOR_BUILD_VERSION"
    else
      BODY="$UPLOADTOOL_BODY"
    fi
  else
    BODY="$UPLOADTOOL_BODY"
  fi

  release_infos=$(curl -s -S -H "Authorization: token ${GITHUB_TOKEN}" \
       --data '{"tag_name": "'"$RELEASE_NAME"'","target_commitish": "'"$APPVEYOR_REPO_COMMIT"'","name": "'"$RELEASE_TITLE"'","body": "'"$BODY"'","draft": false,"prerelease": '$is_prerelease'}' "https://api.github.com/repos/$REPO_SLUG/releases")

  echo "$release_infos"

  unset upload_url
  upload_url=$(echo "$release_infos" | grep '"upload_url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
  echo "upload_url: $upload_url"

  unset release_url
  release_url=$(echo "$release_infos" | grep '"url":' | head -n 1 | cut -d '"' -f 4 | cut -d '{' -f 1)
  echo "release_url: $release_url"

fi # if [ "$APPVEYOR_REPO_COMMIT" != "$tag_sha" ]

if [ -z "$release_url" ] ; then
	echo "Cannot figure out the release URL for $RELEASE_NAME"
	exit 1
fi

echo "Upload binaries to the release..."

for FILE in "$@" ; do
  FULLNAME="${FILE}"
  BASENAME="$(basename "${FILE}")"
  curl -s -S -H "Authorization: token ${GITHUB_TOKEN}" \
       -H "Accept: application/vnd.github.manifold-preview" \
       -H "Content-Type: application/octet-stream" \
       --data-binary @$FULLNAME \
       "$upload_url?name=$BASENAME"
  echo ""
done

$shatool "$@"

if [ "$APPVEYOR_REPO_COMMIT" != "$tag_sha" ] ; then
  echo "Publish the release..."

  release_infos=$(curl -s -S -H "Authorization: token ${GITHUB_TOKEN}" \
       --data '{"draft": false}' "$release_url")

  echo "$release_infos"
fi # if [ "$APPVEYOR_REPO_COMMIT" != "$tag_sha" ]
