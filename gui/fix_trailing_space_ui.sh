#!/bin/bash
for file in *.ui; do
  sed -i -e 's/ <\/string/<\/string/' "$file"
done
