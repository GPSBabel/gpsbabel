#!/bin/bash -x
SOURCE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}" )" && pwd)"

"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.5.0 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.5.1 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.5.2 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.5.3 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.5.4 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.6.0 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.7.0 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.8.0 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v 1.9.0 -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -- -D1
"${SOURCE_DIR}/run_gpsbabel.sh" -v dev -- -D1
