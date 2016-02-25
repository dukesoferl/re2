# Erlang NIF bindings for the re2 regex library

## Building and Installing

**re2** is built with [rebar](https://github.com/rebar/rebar/) and we do expect
`rebar` to be in the search `PATH`.  If `rebar` can not be found in the search
`PATH`, it will be  automatically downloaded to `support/rebar` for local
use.

To build, execute `make` or `rebar compile`.

## re2 sources

If re2 is found on the system, it will be used. Otherwise, the
[re2](https://github.com/google/re2) dependency is automatically downloaded to
`c_src/re2` by the build script.  If you want to ignore the system re2 and use
a local copy, you can set the env var `NO_SYSTEM_RE2`.

Unless otherwise noted, the [re2](https://github.com/google/re2) source files
are distributed under the  BSD-style license found in `c_src/re2/LICENSE`.
