# tidy-tests

[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
![CI](https://github.com/nedervold/tidy-tests/workflows/CI/badge.svg)
<!-- [![Hackage](https://img.shields.io/hackage/v/tidy-tests.svg?logo=haskell)](https://hackage.haskell.org/package/tidy-tests) -->
<!-- [![Stackage Lts](http://stackage.org/package/tidy-tests/badge/lts)](http://stackage.org/lts/package/tidy-tests) -->
<!-- [![Stackage Nightly](http://stackage.org/package/tidy-tests/badge/nightly)](http://stackage.org/nightly/package/tidy-tests) -->

`tidy-tests` is a Unix pipe that appends a snippet of test source to
the corresponding test file.  It is intended to be used with a utility
like `tasty-discover` that automatically collected individual tests
into a test suite.

It is called with a single argument, the path of the module to be
tested.  It searches for the enclosing project directory (identified
by the presence of a Cabal file), then makes a corresponding test
filepath, under `test` instead of `src`, and with name `XxxSpec.hs` if
the source file name is `Xxx.hs`.  It creates the test file and its
parent directories if necessary, then it appends all text from `stdin`
to the test file.
