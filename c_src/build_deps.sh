#!/bin/sh
set -e

test `basename $PWD` != "c_src" && cd c_src

case "$(uname -o)" in
    Msys*|Cygwin*)
        IS_WINDOWS=yes
        ;;
    *)
        IS_WINDOWS=no;;
esac

case "$1" in
    clean)
        rm -rf re2/obj
        ;;

    *)
        if [ x"$IS_WINDOWS" = x"yes" ]; then
            RE2_ARCHIVE=re2.lib
        else
            RE2_ARCHIVE=libre2.a
        fi
        if [ x"$RE2_DEBUG" = x"1" ]; then
            LIBRE2="obj/dbg/$RE2_ARCHIVE"
        else
            LIBRE2="obj/$RE2_ARCHIVE"
        fi
        test -f re2/$LIBRE2 && exit 0

        RE2_REV=${RE2_REV:-2020-01-01}
        case $(git config --get remote.origin.url) in
            git@github.com*|https://github.com*|git://github.com*)
                RE2_DEFAULT_URL=https://github.com/google/re2
                ;;
            *)
                RE2_DEFAULT_URL=https://code.googlesource.com/re2
                ;;
        esac
        RE2_URL=${RE2_URL:-$RE2_DEFAULT_URL}
        test -d re2 || git clone $RE2_URL re2
        (cd re2 && git fetch --all && git checkout $RE2_REV)

        if [ x"$IS_WINDOWS" = x"yes" ]; then
            (
                cd re2
                mkdir -p windows_build
                cd windows_build

                GENERATOR=${RE2_CMAKE_GENERATOR:-"Visual Studio 15 2017"}
                if [ x"$RE2_DEBUG" = x"1" ]; then
                    BUILD_TYPE=Debug
                else
                    BUILD_TYPE=Release
                fi
                LIB=windows_build/$BUILD_TYPE/$RE2_ARCHIVE
                cmake -D RE2_BUILD_TESTING=OFF \
                    -D CMAKE_BUILD_TYPE=$BUILD_TYPE \
                    -G "$GENERATOR" \
                    -D CMAKE_GENERATOR_PLATFORM=x64 \
                    -D CMAKE_COLOR_MAKEFILE=OFF \
                    ..
                cmake --build . --config $BUILD_TYPE
                cd ..
                mkdir -p $(dirname $LIBRE2)
                cp $LIB $LIBRE2
            )
        else
            CXXFLAGS="-Wall -O3 -fPIC -pthread -std=c++11 -m$ERLANG_ARCH"
            CXX="${CXX:-c++} -m$ERLANG_ARCH"
            type gmake 1>/dev/null 2>/dev/null && MAKE=gmake
            MAKE=${MAKE:-make}
            MAKEFLAGS=${MAKEFLAGS:--j2}
            export MAKEFLAGS
            ($MAKE -C re2 $LIBRE2 CXX="$CXX" CXXFLAGS="$CXXFLAGS")
        fi
        ;;
esac
