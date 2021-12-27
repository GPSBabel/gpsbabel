#!/usr/bin/env python3

import argparse
import os
import re

parser = argparse.ArgumentParser(prog='mkstyle')
parser.add_argument('-o', '--out', default='internal_styles.cc',
                    help='output file')
parser.add_argument('style_file', nargs='+')
args = parser.parse_args()

with open(args.out, 'w', encoding='utf-8') as fout:
    print(r'/* This file is machine-generated from the contents of style/   */',
          file=fout)
    print(r'/* by mkstyle.py.  Editing it by hand is an exceedingly bad idea. */',
          file=fout)
    print('', file=fout)
    print(r'#include <QVector>', file=fout)
    print(r'#include "defs.h"', file=fout)
    print(r'#if CSVFMTS_ENABLED', file=fout)

    alist = []
    for file in sorted(args.style_file):
        if file.endswith(".style"):
            a = os.path.splitext(os.path.basename(file))[0]
            alist.insert(0, '{{ \"{0}\", {0} }}'.format(a))
            print('static char {0}[] ='.format(a), file=fout)
            with open(file, 'r', encoding='utf-8') as fin:
                for line in fin:
                    oline = line.rstrip()
                    # escape back slashes
                    oline = oline.replace('\\', '\\\\')
                    # escape double quotes
                    oline = oline.replace('"', '\\"')
                    # start nonempty lines with a double quote
                    oline = re.sub(r'^(.)', r'"\1', oline)
                    # end nonempty lines with a newline escape sequence and a
                    # double quote
                    oline = re.sub(r'(.)$', r'\1\\n"', oline)
                    # indent nonempty lines
                    oline = re.sub(r'^(.)', r'  \1', oline)
                    print(oline, file=fout)
                print('  ;', file=fout)
    separator = ', '
    print(r'const QVector<style_vecs_t> style_list = {{{0}}};'
          .format(separator.join(alist)), file=fout)
    print(r'#else /* CSVFMTS_ENABLED */', file=fout)
    print(r'const QVector<style_vecs_t> style_list;', file=fout)
    print(r'#endif /* CSVFMTS_ENABLED */', file=fout)
