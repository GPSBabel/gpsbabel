#!/bin/bash -ex

# we use a persistent container so the gpsbabel GUI state is saved and restored (in the container).
container_name=tsteven4_ollama

# root needs to run ollama serve&

# if the container doesn't exist create it.
if [ -z "$(docker ps -a -q -f "name=^${container_name}$")" ]; then
  docker run -d --network=host --name "${container_name}" tsteven4/gpsbabel_ollama_server
# this is more restrictive, but seems to hang
#  docker run -d -e OLLAMA_HOST=0.0.0.0:11434 -p 11434:11434 --name "${container_name}" tsteven4/gpsbabel_ollama_server
elif [ -z "$(docker ps -a -q -f "status=running" -f "name=^${container_name}$")" ]; then
  docker start "${container_name}" >/dev/null
fi
