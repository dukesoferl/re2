# Erlang NIF bindings for the re2 regex library

## Building and Installing

**re2** is built with [rebar](http://bitbucket.org/basho/rebar/) and
we do expect `rebar` to be in the search `PATH`.  
If `rebar` can not be found in the search `PATH` it will be  
automatically downloaded to `support/rebar` for local usage.

### To build and run eunit tests execute
`make`  
or  
`rebar compile eunit`

## re2 sources

[re2](http://code.google.com/p/re2/) dependency is automatically downloaded
to `c_src/re2` by the build script.  
Unless otherwise noted, the [re2](http://code.google.com/p/re2/)
source files are distributed under the  
BSD-style license found in `c_src/re2/LICENSE`.
