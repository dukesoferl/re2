# Erlang NIF bindings for the RE2 regex library

## Building and Installing

**re2** is built with [rebar](http://bitbucket.org/basho/rebar/) and
we do expect `rebar` to be in the search `PATH`.  
If `rebar` can not be found in the search `PATH` it will be
automatically downloaded to `support/rebar` for local usage.

### To build and run eunit tests execute
`make`  
or  
`rebar compile eunit`  

## RE2 sources

Contains a bundled copy of the [RE2](http://code.google.com/p/re2/) library.  
Unless otherwise noted, the [RE2](http://code.google.com/p/re2/)
source files are distributed under the  
BSD-style license found in the `c_src/re2/LICENSE` file.
