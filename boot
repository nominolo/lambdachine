#!/bin/sh
AUTOMAKE_VERSION=1.10 AUTOCONF_VERSION=2.68 autoreconf -fi
test -f mk/build.mk || touch mk/build.mk
