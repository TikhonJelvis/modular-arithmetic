# Modular Arithmetic

[![Hackage package](http://img.shields.io/hackage/v/modular-arithmetic.svg)](http://hackage.haskell.org/package/modular-arithmetic)
[![Build Status](https://travis-ci.org/TikhonJelvis/modular-arithmetic.svg?branch=master)](https://travis-ci.org/TikhonJelvis/modular-arithmetic)

This package provides a type for integers modulo some constant, usually written as ℤ/n. 

Here is a quick example:

```
>>> 10 * 11 :: ℤ/7
5
```

It also works correctly with negative numeric literals:

```
>>> (-10) * 11 :: ℤ/7
2
```

Modular division is an inverse of modular multiplication.
It is defined when divisor is coprime to modulus:

```
>>> 7 `div` 3 :: ℤ/16
13
>>> 3 * 13 :: ℤ/16
7
```
