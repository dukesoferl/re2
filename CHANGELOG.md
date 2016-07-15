# Change Log
Noteworthy changes to this project will be documented in this file.

## 1.?.? - 2016-??-??
### Fixed
- Fix rebar3 hooks and make building with rebar3 reliable. Requires
  pc (port_compiler) plugin >=1.3.

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
