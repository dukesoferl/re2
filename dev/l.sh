#!/bin/sh
set -eu
REBAR=`sh -c "PATH=$PATH:dev which rebar3||dev/getrebar||echo false"`
/usr/bin/time erl \
    -pa $($REBAR as debug path --ebin) \
    -noinput \
    -eval "re2:l($1), init:stop()."
