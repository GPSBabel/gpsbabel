#!/bin/bash -e
# load keychain for codesigning
# adapted from
# https://docs.github.com/en/enterprise-cloud@latest/actions/how-tos/deploy/deploy-to-third-party-platforms/sign-xcode-applications

if [ -n "${BUILD_CERTIFICATE_BASE64}" ] && \
   [ -n "${P12_PASSWORD}" ] && \
   [ -n "${KEYCHAIN_PASSWORD}" ]; then
  echo "Importing signing certificate"
  # create variables
  CERTIFICATE_PATH=$RUNNER_TEMP/build_certificate.p12
  KEYCHAIN_PATH=$RUNNER_TEMP/app-signing.keychain-db

  # import certificate and provisioning profile from secrets
  echo -n "$BUILD_CERTIFICATE_BASE64" | base64 --decode -o "$CERTIFICATE_PATH"

  # create temporary keychain
  security create-keychain -p "$KEYCHAIN_PASSWORD" "$KEYCHAIN_PATH"
  security set-keychain-settings -lut 21600 "$KEYCHAIN_PATH"
  security unlock-keychain -p "$KEYCHAIN_PASSWORD" "$KEYCHAIN_PATH"

  # import certificate to keychain
  security import "$CERTIFICATE_PATH" -P "$P12_PASSWORD" -A -t cert -f pkcs12 -k "$KEYCHAIN_PATH"
  security set-key-partition-list -S apple-tool:,apple: -k "$KEYCHAIN_PASSWORD" "$KEYCHAIN_PATH"
  security list-keychain -d user -s "$KEYCHAIN_PATH"
fi
if [ -n "${GITHUB_ENV}" ]; then
  IDENTITY=$(security find-identity -p codesigning "$KEYCHAIN_PATH" | grep '1)' | cut -d '"' -f 2)
  if [ -z "${IDENTITY}" ]; then
    IDENTITY="-"
  fi
  echo "Loaded identity \"$IDENTITY\" for codesigning."
  echo IDENTITY="${IDENTITY}" >> "$GITHUB_ENV"
fi
