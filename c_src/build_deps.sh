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
    ( hg clone https://re2.googlecode.com/hg/ re2 && \
      cd re2 && make -j2)
    ;;
esac
