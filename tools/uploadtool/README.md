# uploadtool [![Build Status](https://travis-ci.org/probonopd/uploadtool.svg?branch=master)](https://travis-ci.org/probonopd/uploadtool)

Super simple uploading of continuous builds (each push) to GitHub Releases. If this is not the easiest way to upload continuous builds to GitHub Releases, then it is a bug.

## Usage

This script is designed to be called from Travis CI after a successful build. By default, this script will _delete_ any pre-existing release tagged with `continuous`, tag the current state with the name `continuous`, create a new release with that name, and upload the specified binaries there. For pull requests, it will upload the binaries to transfer.sh instead and post the resulting download URL to the pull request page on GitHub.

 - On https://github.com/settings/tokens, click on "Generate new token" and generate a token with at least the `public_repo`, `repo:status`, and `repo_deployment` scopes
 - On Travis CI, go to the settings of your project at `https://travis-ci.org/yourusername/yourrepository/settings`
 - Under "Environment Variables", add key `GITHUB_TOKEN` and the token you generated above as the value. **Make sure that "Display value in build log" is set to "OFF"!**
 - In the `.travis.yml` of your GitHub repository, add something like this (assuming the build artifacts to be uploaded are in out/):

```yaml
after_success:
  - ls -lh out/* # Assuming you have some files in out/ that you would like to upload
  - wget -c https://github.com/probonopd/uploadtool/raw/master/upload.sh
  - bash upload.sh out/*

branches:
  except:
    - # Do not build tags that we create when we upload to GitHub Releases
    - /^(?i:continuous)$/
```

## Environment variables

`upload.sh` normally only creates one stream of continuous releases for the latest commits that are pushed into (or merged into) the repository.

It's possible to use `upload.sh` in a more complex manner by setting the environment variable `UPLOADTOOL_SUFFIX`. If this variable is set to the name of the current tag, then `upload.sh` will upload a release to the repository (basically reproducing the `deploy:` feature in `.travis.yml`).

If `UPLOADTOOL_SUFFIX` is set to a different text, then this text is used as suffix for the `continuous` tag that is created for continuous releases. This way, a project can customize what releases are being created.
One possible use case for this is to set up continuous builds for feature or test branches:
```
  if [ ! -z $TRAVIS_BRANCH ] && [ "$TRAVIS_BRANCH" != "master" ] ; then
    export UPLOADTOOL_SUFFIX=$TRAVIS_BRANCH
  fi
```
This will create builds tagged with `continuous` for pushes / merges to `master` and with `continuous-<branch-name>` for pushes / merges to other branches.

The two environment variables `UPLOADTOOL_PR_BODY` and `UPLOADTOOL_BODY` allow the calling script to customize the messages that are posted either for pull requests or merges / pushes. If these variables aren't set, generic default texts are used.
