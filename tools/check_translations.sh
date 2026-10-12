#!/bin/bash -e
#  --no-accelerator     Disable the accelerator check
#  --no-punctuation     Disable the punctuation check
#  --no-place-marker    Disable the place marker check
#  --no-whitespaces     Disable the check for surrounding white spaces

declare -A check_hash=(
    [accelerator]="--no-punctuation --no-place-marker --no-whitespaces"
    [punctuation]="--no-accelerator --no-place-marker --no-whitespaces"
    [place-marker]="--no-accelerator --no-punctuation --no-whitespaces"
    [whitespaces]="--no-accelerator --no-punctuation --no-place-marker"
)
for check in "${!check_hash[@]}"; do
  for file in gpsbabel*ts; do
    echo -n "$check: $file: "
    # shellcheck disable=SC2086 # Intentional word splitting
    lcheck ${check_hash[$check]} -o lcheck."$check"."$(basename -s .ts "$file")" "$file" || true
  done
done
