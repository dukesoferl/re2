# Erlang NIF bindings for the re2 regex library

## Building and Installing

**re2** is built with [rebar](https://github.com/rebar/rebar/) and we do expect
`rebar` to be in the search `PATH`. If `rebar` can not be found in the search
`PATH`, it will be automatically downloaded to `support/rebar` for local use.

To build, execute `make` or `rebar compile`.

## re2 sources

If re2 is found on the system, it will be used. Otherwise, the
[re2](https://github.com/google/re2) dependency is automatically downloaded to
`c_src/re2` by the build script. If you prefer to link against re2 as found on
the system, you can set the env var `SYSTEM_RE2`. If you set `SYSTEM_RE2` and
the library can not be found, it will fall back to a local copy, as if you
didn't use the env var.

Unless otherwise noted, the [re2](https://github.com/google/re2) source files
are distributed under the  BSD-style license found in `c_src/re2/LICENSE`.
