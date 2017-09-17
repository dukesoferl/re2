# Erlang NIF bindings for the RE2 regex library

[![GitLab-CI Build Status](https://gitlab.com/tuncer/re2erl/badges/master/pipeline.svg)](https://gitlab.com/tuncer/re2erl/pipelines)

## Using re2

The library's API follows the standard Erlang/OTP `re` API as closely as possible
while accounting for the differences in RE2. One notable difference is that
there's no `run/{2,3}` but only `match/{2,3}` as in the C++ API. It would
be easy to add `run/{2,3}`, if that's needed.

## Obtaining re2

### Installation via package manager

To use `re2`, you can add it as a project dependency and let your
package manager of choice handle it:

rebar.config: `{re2, "1.*"}`

erlang.mk: `DEPS = re2`

mix.exs: `{:re2, "~> 1.*"}`

### Installation from source into `$ERL_LIBS`

If you want to make `re2` available globally, you can install it from source
into your Erlang installation by adding it in one of your `$ERL_LIBS` paths.
So, it's either somewhere like `/usr/local/lib/lib/erlang/lib` or `$HOME/.erl`.

You can either download a tagged release from
`https://github.com/tuncer/re2/releases` and extract that or clone the git
repo `https://github.com/tuncer/re2` in the target directory. Once that's
done, cd into the directory and run `rebar compile` or just `make`.

Now, if you start `erl`, you should be able to call functions from the
`re2` module.

```
$ erl
1> code:which(re2).
"/usr/local/lib/erlang/lib/re2/ebin/re2.beam"
2>
```
### Advanced build time options

If RE2 is found on the system, it will be used. Otherwise, the
[RE2](https://github.com/google/re2) dependency is automatically downloaded to
`c_src/re2` by the build script. If you prefer to link against RE2 as found on
the system, you can set the env var `SYSTEM_RE2=1`. If you set `SYSTEM_RE2=1` and
the library can not be found, it will fall back to a local copy, as if you
didn't use the env var. If you want to override the RE2 version that is fetched
and built, when not using system RE2, you can do so by setting `RE2_REV` to a
git rev.

Unless otherwise noted, the [RE2](https://github.com/google/re2) source files
are distributed under the BSD-style license found in `c_src/re2/LICENSE`.
