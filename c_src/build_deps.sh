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

    ERL_ARCH=`erl -noinput +B -eval \
      'io:format("~B",[8 * erlang:system_info(wordsize)]),halt(0).'`
    CXXFLAGS="-Wall -O3 -fPIC"

    if [ $ERL_ARCH -eq 32 ]; then
      CXXFLAGS="$CXXFLAGS -m32"
    fi

    (cd re2 && make -j2 CXXFLAGS="$CXXFLAGS")

    ;;
esac
