#!/bin/sh
set -eu
/usr/bin/time erl \
    -pa $(rebar3 as debug path --ebin) \
    -noinput \
    -eval "re2:l($1), init:stop()."
