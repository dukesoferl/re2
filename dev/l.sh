#!/bin/sh
set -eu
/usr/bin/time erl -pa ebin -noinput -eval "re2:l($1), init:stop()."
