Erlang NIF bindings for the RE2 regex library
=============================================


[![Build Status](https://dev.azure.com/dukesoferl/re2erl/_apis/build/status/dukesoferl.re2?branchName=master)](https://dev.azure.com/dukesoferl/re2erl/_build/latest?definitionId=1&branchName=master)

Using re2
---------

The library\'s API follows the standard Erlang/OTP `re` API as closely
as possible while accounting for the differences in RE2:

```
$ erl
1> re2:run("Bar-foo-Baz", "FoO", [caseless]).
{match,[<<"foo">>]}
2> re2:replace("Baz-foo-Bar", "foo", "FoO", []).
<<"Baz-FoO-Bar">>
3> {ok, RE} = re2:compile("Foo.*Bar", [caseless]).
{ok,#Ref<0.3540238268.2241986568.233969>}
4> re2:run("Foo-baz-bAr", RE).
{match,[<<"Foo-baz-bAr">>]}
```

Obtaining re2
-------------

### Installation via package manager

To use `re2`, you can add it as a project dependency and let your
package manager of choice handle it:

| Build tool   | Dependency spec    |
| ----------   | ---------------    |
| rebar.config | `{deps, [re2]}`    |
| erlang.mk    | `DEPS = re2`       |
| mix.exs      | `{:re2, "~> 1.*"}` |

### Installation from source into `$ERL_LIBS`

If you want to make `re2` available globally, you can install it from
source into your Erlang installation by adding it in one of your
`$ERL_LIBS` paths. So, it\'s either somewhere like `/usr/lib/erlang/lib`
or `$HOME/.erl`.

You can either download a [tagged release](https://github.com/tuncer/re2/releases) or clone
the [git repo](https://github.com/tuncer/re2) in the target directory. Once
that\'s done, cd into the directory and run `rebar3 compile` or just
`make`.

Now, if you start `erl`, you should be able to call functions from the
`re2` module:

```
$ erl
1> code:which(re2).
"/usr/lib/erlang/lib/re2/ebin/re2.beam"
2>
```

### Advanced build time options

[RE2](https://github.com/google/re2) is automatically downloaded to
`c_src/re2` by the build script, and linked into the NIF lib. If you
prefer to link against [RE2](https://github.com/google/re2) as found on
the system, you can set the env var `SYSTEM_RE2=1`. If you do that and
the library can not be found, it will fall back to a local copy
(`c_src/re2`). Also, if you want to override the RE2 version that is
fetched and built, when not using system RE2, you can do so by setting
`RE2_REV` to a git rev.

By default, RE2 upstream source is fetched from the Google remote. If
for some reason you need to fetch the upstream source from some other
git repository, you can do so by setting the `RE2_URL` environment
variable to a different git url.

### Windows build

If you're trying to build on Windows, please make sure you have CMake and
Visual Studio 2017. Before trying to build with `rebar3`, make sure
`rebar3`, `erlc`, `make`, `cmake` are in the search path and that you've
run Visual Studio's `vcvars64.bat`, or are inside the equivalent shell
(spawned from the start menu entry). Windows builds are tested on Azure
Pipelines (see CI badge).

License
-------

Unless otherwise noted, the [RE2](https://github.com/google/re2) source
files are distributed under the BSD-style license found in
[c\_src/re2/LICENSE](https://raw.githubusercontent.com/google/re2/master/LICENSE).
The same
[license](https://raw.githubusercontent.com/tuncer/re2/master/LICENSE)
is used for the NIF library.
