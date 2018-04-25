#!/bin/sh
set -eu
/usr/bin/time erl \
    -pa _build/debug/lib/re2/ebin \
    -noinput \
    -eval "re2:l($1), init:stop()."
