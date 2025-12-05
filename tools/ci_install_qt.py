#!/bin/python3
"""
Copyright (C) 2025 Robert Lipe, robertlipe+source@gpsbabel.org

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
"""

import argparse
import os
from pathlib import Path
import platform
import re
import shutil
import stat
import subprocess
import sys
import urllib.request
import xml.etree.ElementTree as ET

rejectlicense = [
    "qtcharts",
    "qtdatavis3d",
    "qtgraphs",
    "qtgrpc",
    "qthttpserver",
    "qtlottie",
    "qtnetworkauth",
    "qtquick3d",
    "qtquick3dphysics",
    "qtquicktimeline",
    "qtwebglplugin",
    "qtshadertools",
    "qtvirtualkeyboard",
    "qtwaylandcompositor",
]
unused = [
    "qtinsighttracker",  # extension
    "qt3d",
    "qtactiveqt",
    "qtconnectivity",
    "qtlanguageserver",
    "qtlocation",
    "qtmultimedia",
    "qtquickeffectmaker",
    "qtremoteobjects",
    "qtscxml",
    "qtsensors",
    "qtspeech",
    "qtwebsockets",
]
black = []
black.extend(rejectlicense)
black.extend(unused)


def fetch_installer(verbose):
    """Fetch the appropriate Qt online installer."""
    installer = None
    if platform.system() == "Windows":
        if platform.machine() in ["x86_64", "AMD64"]:
            installer = "qt-online-installer-windows-x64-online.exe"
        elif platform.machine() in ["arm64", "ARM64"]:
            installer = "qt-online-installer-windows-arm64-online.exe"
    elif platform.system() == "Darwin":
        # note this may require rosetta is installed on arm64 macs.
        installer = "qt-online-installer-mac-x64-online.dmg"
    elif platform.system() == "Linux":
        if platform.machine() == "x86_64":
            installer = "qt-online-installer-linux-x64-online.run"
        elif platform.machine() in ["arm64", "ARM64"]:
            installer = "qt-online-installer-linux-arm64-online.run"
    if installer is None:
        sys.exit(
            f"Error: Unknown installer for system {platform.system()} and machine {platform.machine()}"
        )
    try:
        url = "https://download.qt.io/official_releases/online_installers/" + installer
        urllib.request.urlretrieve(url, installer)
        print(
            f"Fetched installer {installer} for system {platform.system()} and machine {platform.machine()} successfully."
        )
    except urllib.error.URLError as e:
        sys.exit(f"Failed to download file: {e}")
    if platform.system() == "Darwin":
        output = subprocess.run(
            ["hdiutil", "attach", installer],
            capture_output=True,
            text=True,
            encoding="UTF-8",
            check=True,
        )
        volumes = re.findall(
            r"^.*qt-online-installer.*$", output.stdout, flags=re.MULTILINE
        )
        if len(volumes) != 1:
            sys.exit(f"Error: couldn't find macos volume in {volumes}")
        volume = Path(volumes[0].split("\t")[2])
        apps = list(volume.glob("*.app"))
        if len(apps) != 1:
            sys.exit(f"Error: couldn't find macos app in {apps}")
        app = apps[0].name
        exe = apps[0].stem
        apppath = volume / app
        tgtpath = os.path.abspath(os.path.join(app, "Contents", "MacOS", exe))
        if verbose:
            print("Volume is", volume)
            print("App is", app)
            print("App path is", apppath)
            print("Installed installer is", tgtpath)
        shutil.copytree(apppath, app)
        subprocess.run(["hdiutil", "detach", volume, "-quiet"], check=True)
        return tgtpath
    st = os.stat(installer)
    os.chmod(installer, st.st_mode | (stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH))
    return os.path.abspath(installer)


def get_available_pkgs(installer, ver):
    """Find the available packages for this Qt version."""
    verparts = ver.split(".")
    try:
        output = subprocess.run(
            [
                installer,
                "se",
                "--type",
                "package",
                "--filter-packages",
                f"Version=^{verparts[0]}\\.{verparts[1]}\\.{verparts[2]}-",
            ],
            capture_output=True,
            text=True,
            encoding="UTF-8",
            check=True,
        )
    except subprocess.CalledProcessError as e:
        sys.exit(
            f"""Error: Installer failed, rc: {e.returncode}
            Command: {e.cmd}
            stderr: {e.stderr}
            stdout: {e.stdout}"""
        )

    pkgxml = "\n".join(re.findall(r"^(?!\[).*$", output.stdout, flags=re.MULTILINE))
    return pkgxml


def select_pkgs(packagesxml, arch, verbose):
    """Select the packages we want to install."""
    tree = ET.fromstring(packagesxml)
    names = [pkg.get("name") for pkg in tree.findall("package")]
    if verbose:
        print(sorted(names))
    namebits = [name.split(".") for name in sorted(names)]
    prevbits = [None, None, None, None, None]
    leafs = []
    for bits in namebits:
        plen = len(prevbits)
        blen = len(bits)
        if blen == plen + 1 and bits[0:plen] == prevbits[0:plen]:
            if verbose:
                print("internal node", prevbits)
            leafs.pop()
        leafs.append(bits)
        prevbits = bits
    if verbose:
        print(f"found {len(leafs)} leafs from {len(namebits)} nodes.")
    selected = []
    others = 0
    for node in leafs:
        if node[0] == "extensions":
            if node[1] in black or node[-1] != arch:
                if verbose:
                    print(f"extension {node}")
            else:
                if verbose:
                    print(f"extension {node} *")
                selected.append(".".join(node))
        # a few modules in 6.2.4, 6.5.3 weren't in addons, e.g. qt5compat
        elif node[-1].startswith("qt"):
            if node[-1] in black:
                if verbose:
                    print(f"module {node}")
            else:
                if verbose:
                    print(f"module {node} *")
                selected.append(".".join(node))
        else:
            if node[-1] == arch:
                if verbose:
                    print(f"other {node} *")
                selected.append(".".join(node))
                others += 1
            else:
                if verbose:
                    print(f"other {node}")
    if others != 1:
        sys.exit(f"Error: couldn't find architecture {arch}")
    print(f"Selected packages are {selected}.")
    return " ".join(selected)


def install(installer, dest, selected):
    """Install the selected packages."""
    installargs = [
        installer,
        "--root",
        dest,
        "--accept-licenses",
        "--accept-obligations",
        "--default-answer",
        "--confirm-command",
        "--no-force-installations",
        "install",
    ]
    installargs.extend(selected.split())
    subprocess.run(installargs, check=True)


def cleanup(dest, ver):
    """Delete all the stuff we don't want that the Qt installer insists on installing."""
    dpath = Path(dest)
    vpath = dpath / ver
    for entry in dpath.iterdir():
        if not entry.samefile(vpath):
            if entry.is_file():
                print(f"removing file {entry}")
                entry.unlink()
            elif entry.is_dir():
                shutil.rmtree(entry)
                print(f"removing dir {entry}")
            else:
                print(f"unknown directory entry {entry}")


def main():
    """Install Qt."""
    parser = argparse.ArgumentParser(
        prog="ci_install_qt",
        description="Install Qt using official online installer",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument("ver", help="version, e.g. 6.10.1")
    parser.add_argument(
        "arch",
        help="architecture, e.g. win64_msvc2022_64, win64_msvc2022_arm64, linux_gcc_64, clang_64",
    )
    parser.add_argument("dest", help="install root, e.g. C:\\Qt")
    parser.add_argument(
        "--verbose",
        "-v",
        help="verbose",
        action="store_const",
        const=True,
        default=False,
    )
    args = parser.parse_args()

    jwt = os.getenv("QT_INSTALLER_JWT_TOKEN")
    if jwt is None or len(jwt) == 0:
        print("Warning: QT_INSTALLER_JWT_TOKEN not set")
    installer = fetch_installer(verbose=args.verbose)
    packagexml = get_available_pkgs(installer=installer, ver=args.ver)
    selected = select_pkgs(packagesxml=packagexml, arch=args.arch, verbose=args.verbose)
    install(installer=installer, dest=args.dest, selected=selected)
    cleanup(dest=args.dest, ver=args.ver)


if __name__ == "__main__":
    main()
