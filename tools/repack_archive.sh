#!/bin/bash -ex
sha=$1
release=$2

outputdir=$(mktemp -d)
tmpdir=$(mktemp -d)

pushd "${outputdir}"
wget "https://github.com/gpsbabel/gpsbabel/archive/${sha}.tar.gz"

archivemount "${sha}.tar.gz" "${tmpdir}"
tar --transform="s,^gpsbabel-${sha},gpsbabel-${release}," -C "${tmpdir}" -czvf "gpsbabel-${release}.tar.gz" "gpsbabel-${sha}"
umount "${tmpdir}"
rmdir "${tmpdir}"

popd
mv "${outputdir}/gpsbabel-${release}.tar.gz" "gpsbabel-${release}.tar.gz"
rm -fr "${outputdir}"
