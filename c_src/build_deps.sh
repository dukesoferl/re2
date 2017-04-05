#!/bin/sh

set -e

test `basename $PWD` != "c_src" && cd c_src

case "$1" in
  clean)
    rm -rf re2/obj
    ;;

  *)
    if [ x"$DEBUG" = x"1" ]; then
        LIBRE2="obj/dbg/libre2.a"
    else
        LIBRE2="obj/libre2.a"
    fi
    test -f re2/$LIBRE2 && exit 0

    RE2_REV=${RE2_REV:-2017-03-01}
    test -d re2 || git clone https://github.com/google/re2
    (cd re2 && git fetch --all && git checkout $RE2_REV)

    CXXFLAGS="-Wall -O3 -fPIC -pthread -std=c++11 -m$ERLANG_ARCH"
    CXX="${CXX:-c++} -m$ERLANG_ARCH"
    which gmake 1>/dev/null 2>/dev/null && MAKE=gmake
    MAKE=${MAKE:-make}
    MAKEFLAGS=${MAKEFLAGS:--j2}
    export MAKEFLAGS
    ($MAKE -C re2 $LIBRE2 CXX="$CXX" CXXFLAGS="$CXXFLAGS")
    ;;
esac
