#!/usr/bin/env python3
import urllib.parse, sys;
print(urllib.parse.quote(sys.argv[1]), end='')
