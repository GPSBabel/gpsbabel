#!/bin/bash -ex

CODACY_URL="https://api.codacy.com"
COMMIT=$(git log -1 --format='%H')
CODACY_CLANG_TIDY=$(curl -s https://api.github.com/repos/codacy/codacy-clang-tidy/releases/latest | jq '.assets[] | select(.name|startswith("codacy-clang-tidy-linux-")) | .browser_download_url' | tr -d \")

CHECKS="clang-diagnostic-*,clang-analyzer-*,cppcoreguidelines-*,modernize-*,bugprone-*,google-*,misc-*,performance-*,readability-*,-cppcoreguidelines-pro-type-vararg,-modernize-use-trailing-return-type,-readability-identifier-length"
HEADERFILTER=".*"

mkdir bld-tidy
cd bld-tidy
cmake -G Ninja -DCMAKE_BUILD_TYPE=Release -DGPSBABEL_ENABLE_PCH=OFF -DCMAKE_EXPORT_COMPILE_COMMANDS=ON ..
# generate included ui files
cmake --build .
cd ..

# exclude third party library source
jq '[.[]|select(.file|contains("zlib")|not)] | [.[]|select(.file|contains("shapelib")|not)] | [.[]|select(.file|contains("bld-tidy")|not)]' \
bld-tidy/compile_commands.json \
> compile_commands.json

# run-clang-tidy may still be forcing injection of escape sequences for colors.
# this will cause codacy-clang-tidy to not find anything.
sed "s/, '--use-color'//" "$(which run-clang-tidy)" > run-clang-tidy-nocolor
chmod +x run-clang-tidy-nocolor
./run-clang-tidy-nocolor -p "$(pwd)" -header-filter "${HEADERFILTER}" -checks "${CHECKS}" | \
tee tidy.out

curl -L "${CODACY_CLANG_TIDY}" --output codacy-clang-tidy
chmod +x codacy-clang-tidy


./codacy-clang-tidy < tidy.out > tidy.report

# don't leak secrets
set +x
curl -XPOST -L -H "project-token: $CODACY_PROJECT_TOKEN" \
    -H "Content-type: application/json" -d @tidy.report \
    "$CODACY_URL/2.0/commit/$COMMIT/issuesRemoteResults"

curl -XPOST -L -H "project-token: $CODACY_PROJECT_TOKEN" \
        -H "Content-type: application/json" \
        "$CODACY_URL/2.0/commit/$COMMIT/resultsFinal"

