#!/bin/bash -e
#
# cppcheck 2.19.0, 2.22.0 wrongly detect a syntaxError on bitfields with sizes and brace initializers, .e.g 'unsigned int shortname_is_synthetic:1{0};'
# This requires	modification to defs.h, garmin_fs.h, gbfile.h, and geocache.h to avoid.
#
cppcheck --project=bld/compile_commands.json --enable=all --checkers-report=cpc.report -i jeeps -i gui -i zlib -i shapelib -i strptime --std=c++20 --check-level=exhaustive -DQT_VERSION_CHECK\(a,b,c\)=0 |& tee cpc.log
grep '\[' cpc.log | grep '\]$' | sed 's/.* \[/[/' | sort | uniq -c | sort -n | tee cpc.summary
