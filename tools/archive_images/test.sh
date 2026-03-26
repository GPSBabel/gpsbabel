#!/bin/bash -ex
# run testo on the docker images

versions=(1.5.0 1.5.1 1.5.2 1.5.3 1.5.4 1.6.0 1.7.0 1.8.0 1.9.0 1.10.0)
for ver in "${versions[@]}"; do
  ver_=$(echo "${ver}" | tr . _) 
  tag=gpsbabel_${ver_}
  TMPDIR=$(mktemp -d)
  trap 'rm -fr ${TMPDIR}' 0 1 2 3 15
  pushd "${TMPDIR}"
  git clone --depth 1 --branch "${tag}" https://github.com/tsteven4/gpsbabel.git
  cd gpsbabel
  # early releases had testo under directory gpsbabel
  if [ -d gpsbabel ]; then
    cd gpsbabel
  fi 
  # run testo, using testo, the tests and references from the DUT.
  # some tests failed before 1.6.0
  if [[ "${ver}" == "dev" || $(echo -e "${ver}\n1.6.0" | sort -V | head -n1) == "1.6.0" ]]; then
    # ver = dev, or ver >= 1.6.0
    docker run -it --rm -v "$(pwd):/app" "tsteven4/gpsbabel:${ver}" /bin/bash -c 'PNAME=/usr/local/bin/gpsbabel ./testo'
  else
    # ver < 1.6.0
    docker run -it --rm -v "$(pwd):/app" "tsteven4/gpsbabel:${ver}" /bin/bash -c 'PNAME=/usr/local/bin/gpsbabel ./testo || true'
  fi
  popd
  rm -fr "${TMPDIR}"
done
