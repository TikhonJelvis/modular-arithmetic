# Modular Arithmetic

[![Hackage package](http://img.shields.io/hackage/v/modular-arithmetic.svg)](http://hackage.haskell.org/package/modular-arithmetic)
[![Build Status](https://travis-ci.org/TikhonJelvis/modular-arithmetic.svg?branch=master)](https://travis-ci.org/TikhonJelvis/modular-arithmetic)

This package provides a type for integers modulo some constant, usually written as ℤ/n. 

Here is a quick example:

```
*Data.Modular> (10 :: ℤ/7) * (11 :: ℤ/7)
5
```

It also works correctly with negative numeric literals:

```
*Data.Modular> (-10 :: ℤ/7) * (11 :: ℤ/7)
2
```
