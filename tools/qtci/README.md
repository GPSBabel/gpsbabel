# QT-CI
This project collects a set of script for building Qt application for Android/iOS in command line environment.

[![Build Status](https://travis-ci.org/benlau/qtci.svg?branch=master)](https://travis-ci.org/benlau/qtci)

Check [.travis.yml](https://github.com/benlau/qtci/blob/master/.travis.yml) to see how it works.
It will demonstrate how to build an apk file using QT-CI scripts.

Installation
============

Since this project is a collection of scripts, and the script in the bin folder does not have any dependence on each other.
It is not necessary to clone/download the whole repository into your build environment.
You may simply copy the script you need from this repository.

**recipes/install-qt**

To automatically install Qt, you can just download 2 scripts and grant them permission for execution.

"recipes/install-qt"
"bin/extract-qt-installer"

Then just run script "recipes/install-qt" with the desired version of Qt
Example:

    bash install-qt 5.9.4

Enviroument variables
=====================

QT_CI_PACKAGES - packages to install. You can check available packages if set VERBOSE to 1.

Example:

    export VERBOSE=1
    export QT_CI_PACKAGES=qt,qt.594,qt.594.gcc_64,qt.594.doc.qtvirtualkeyboard

Setup
=====

    git clone https://github.com/benlau/qtci.git

    source qtci/path.env #Add qtci/bin and qtci/recipes to $PATH


Script
======

**(1) bin/extract-qt-installer**
--------------------------------

Usage

```
    extract-qt-installer [--disable-progress-report] qt-installer output_path
    extract-qt-installer --list-packages qt-installer
```

Extract Qt from the installer to the target path (for Qt 5.5 or above). If --list-packages is given, it will show the available packages from the installer and terminate immediately. 

Example:

    extract-qt-installer qt-opensource-linux-x64-android-5.5.1.run ~/Qt

**Remarks: The installation path must be absolute path**

Environment Variables

    VERBOSE [Optional] Set to "true" will enable VERBOSE output
    QT_CI_PACKAGES [Optional] Select the components to be installed instead of using default (eg. QT_CI_PACKAGES="qt.59.gcc_64")
    QT_CI_LOGIN [Optional] The login name 
    QT_CI_PASSWORD [Optional] The password of login

The arguments and environment required could be different due to the installer changes. Check the recipes folder or the wiki of known issues to find out the correct setting.

**(2) bin/extract-ifw**
--------------------------------

Extract installer of "Qt installer framework" to the target path

Example:

    extract-ifw qt-installer-framework-opensource-1.5.0-x64.run ~/QtIfw

**(3) bin/install-android-sdk**
--------------------------------

Download and install Android SDK

Example:

    install-android-sdk platform-tool,build-tools-20.0.0,android-19

**(4) bin/build-android-gradle-project**
--------------------------------

Build a Qt Android project and sign the APK

Usage:

    build-android-gradle-project project.pro

Required Environment Variables

    QT_HOME [Required] The home directory of installed Qt. (e.g., ~/Qt/5.7)
    KEYSTORE [Optional] The location of keystore. If it is set, it will be used to sign the created apk
    KEYALIAS [Optional] The alias of the keystore
    KEYPASS  [Optional] The password of keystore.
        ANDROID_TARGET_SDK_VERSION [Optional] Target Android SDK version. The default value is "19"

(5) bin/increase-android-version-code
--------------------------------

Usage

    increase-android-version-code AndroidManifest.xml

Given an AndroidManifest.xml file, it will increase the value of the versionCode field by one.

(6) bin/run-unittests
----------------------

Usage

    run-unittests project.pro

Build and run a unit test project in the current folder. If qpm.json were found, it would call `qpm install` first.

Recipes
=======


In the folder "recipes", it contains a set of script that could download and install specific Qt toolchains for a different environment. (Include Android)

Please feel free to modify and submit a new recipe.

Example

    apt-get install openjdk-8-jdk p7zip

    source path.env #Add $PWD/bin and $PWD/recipes to $PATH

    #Change to the installation path

    qt-5.5.1-android-19 # Install Qt 5.5.1 and Android SDK
    
    source qt-5.5.1-android-19.env # Add installed Qt path to $PATH


Reference
=========

 1. [Continuous distribution for Qt applications on desktop and mobile](http://www.slidedeck.io/lasconic/qtci-qtcon2016)
 1. [Andrew's Articles - Continuous deployment for Qt applications](http://andrewdolby.com/articles/2016/continuous-deployment-for-qt-applications/)

Related Projects
=================

 1. [benlau/quickpromise](https://github.com/benlau/quickpromise) - Promise library for QML
 2. [benlau/quickcross](https://github.com/benlau/quickcross) - QML Cross Platform Utility Library
 3. [benlau/qsyncable](https://github.com/benlau/qsyncable) - Synchronize data between models
 4. [benlau/testable](https://github.com/benlau/testable) - QML Unit Test Utilities
 5. [benlau/quickflux](https://github.com/benlau/quickflux) - Message Dispatcher / Queue for Qt/QML
 6. [benlau/biginteger](https://github.com/benlau/biginteger) - QML BigInteger library
