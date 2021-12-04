#!/usr/bin/sh

set -xe

CFLAGS="-Wall -Wextra -pedantic -ggdb"

cc -o $1 "$1.c" $CFLAGS
