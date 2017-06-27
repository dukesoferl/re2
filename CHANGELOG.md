# Change Log
Noteworthy changes to this project will be documented in this file.

## 1.7.6 - 2017-06-27
### Fixed

- getrebar: use more portable chmod

### Changed

- Switch RE2 default revision to 2017-06-01.

## 1.7.5 - 2017-04-09
### Changed

- Conditionally get RE2 from github.com/google/re2: If the project has been
  cloned from github.com/tuncer/re2, then it's safe to fetch RE2 from
  github.com/google/re2. Otherwise, use code.googlesource.com/re2.

## 1.7.4 - 2017-04-02
### Changed

- Switch RE2 default revision to 2017-04-01.

## 1.7.3 - 2017-04-02
### Changed

- Switch RE2 default revision to 2017-03-01.

## 1.7.2 - 2017-01-28
### Fixed

- Implement kludge to support evaluation of rebar.config.script by Elixir mix.

## 1.7.1 - 2017-01-11
### Changed

- Switch RE2 default revision to 2017-01-01.
- Print debug messages with re2 prefix.
- Teach make debug to enable -g and link dbg RE2.

## 1.7.0 - 2016-12-22
### Changed

- Make use of cpu-bound dirty schedulers if supported and online.
- Optimize capturing group request if NONE or FIRST. While at it, improve
  relevant code comments.
- Refactor RE2 object pointer handling to use std::unique_ptr because it's
  available now that we depend on C++11 due to RE2 upstream.
- Use C++11 vec.data() instead of &vec[0] since it's now available.
- Switch to upstream RE2 2016-11-01 as default RE2 revision.

## 1.6.0 - 2016-10-08
### Fixed

- Fix memory leak with literal regexes. Thanks to reports by Mark Peng, Mathieu
  D'Amours and bkolodziej.

## 1.5.0 - 2016-08-29
### Fixed
- build: fix rebar3 hooks and make building with rebar3 reliable. Requires pc
  (port_compiler) plugin >=1.3.

### Changed
- build: adapt port env to fixed rebar port compiler plugin, which now
  correctly uses CXX to link and not CC. That means, we don't explicitly pass
  -lc++ or -lstdc++ anymore because the link command does the right thing for
  C++ now. This worked in the past, but with the introduction of C++11 due to
  upstream, and the need for a newer toolchain, this started to affect certain
  environments like CentOS. Using CXX as the compile driver is the correct
  thing to do, and it fixes the CentOS issue. This was fixed in rebar 2.6.3,
  but because it works correctly for almost everywhere but environments like
  CentOS, rebar.config.script supports older rebar versions as well.
- build: allow upstream re2 git rev override via RE2_REV env var.
- build: instead of hard-coding -j2, use MAKEFLAGS and of course respect it if
  already set.
- Add GitLab CI config.

## 1.4.1 - 2016-07-12
### Fixed
- Consider libre2.dylib when looking for system re2.

### Changed
- Switch default upstream re2 to 2016-06-01.

## 1.4.0 - 2016-03-14
### Fixed
- Link against libc++ when appropriate.
- Build re2_nif.so with -std=c++11 due to upstream requirement. This
  is in addition to the recent change to build the local re2 in C++11
  mode.

### Changed
- Allow linking against system libre2, if env var SYSTEM_RE2 is set,
  and fall back to local re2 if not found.
- Allow overriding pinned re2 upstream git revision via RE2_REV env var.
- Implement rebar3 support.

## 1.3.1 - 2016-02-11
### Changed
- Use common single dash -std=c++11 instead of --std=c++11.

## 1.3.0 - 2016-02-11
### Fixed
- Adapt to upstream requirement to build re2 in C++11 mode.

## 1.2.2 - 2016-02-11
### Changed
- Pin last upstream re2 release before C++11 was introduced.

## 1.2.1 - 2015-05-12
### Fixed
- Add nif fun entry macro to adapt to NIF API changes.

## 1.2 - 2015-04-19
### Fixed
- Adapt to upstream re2's move to git.

### Added
- Add QuickCheck properties.

## 1.1 - 2014-11-25
### Fixed
- Make a small efficiency improvement in re2:replace.
- Update tests and specs.

### Changed
- Remove dead code.
- Fix minor issues in getrebar escript.

## 1.0 - 2014-11-25
- First official release tag, but the library has been stable for years.
