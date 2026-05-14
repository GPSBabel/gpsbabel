#!/bin/sh -ex

xcrun actool images/macosx/AppIcon.icon --compile images/macosx --app-icon AppIcon --platform macosx --target-device mac --minimum-deployment-target 10.12 --include-all-app-icons --output-partial-info-plist /dev/null --output-format human-readable-text
