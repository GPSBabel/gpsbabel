#!/bin/sh -ex

xcrun actool images/macos/AppIcon.icon --compile images/macos --app-icon AppIcon --platform macosx --target-device mac --minimum-deployment-target 10.12 --include-all-app-icons --output-partial-info-plist /dev/null --output-format human-readable-text
