#!/bin/bash

set -e

if [ `basename $PWD` != "c_src" ]; then
  pushd c_src
fi

case "$1" in
  clean)
    rm -rf re2/obj/*
    ;;

  *)
    (test -d re2 || hg clone https://re2.googlecode.com/hg/ re2)

    ERL_ARCH=`erl -noinput -eval \
      'io:format("~B",[8 * erlang:system_info(wordsize)]),halt(0).'`
    CXXFLAGS="-Wall -O3 -fPIC -pthread"
    LDFLAGS="-pthread"
    MAKE_SHARED_LIBRARY="g++"

    if [ `uname -s` = Darwin ]; then
      MAKE_SHARED_LIBRARY="$MAKE_SHARED_LIBRARY -dynamiclib $LDFLAGS \
        -exported_symbols_list libre2.symbols.darwin"
    else
      MAKE_SHARED_LIBRARY="$MAKE_SHARED_LIBRARY -shared \
        -Wl,-soname,libre2.so.0,--version-script=libre2.symbols $LDFLAGS"
    fi

    if [ $ERL_ARCH -eq 32 ]; then
      CXXFLAGS="$CXXFLAGS -m32"
      MAKE_SHARED_LIBRARY="$MAKE_SHARED_LIBRARY -m32"
    fi

    (cd re2 && make -j2 CXXFLAGS="$CXXFLAGS" LDFLAGS="$LDFLAGS" \
      MAKE_SHARED_LIBRARY="$MAKE_SHARED_LIBRARY")

    ;;
esac
