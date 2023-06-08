2.0.0.3
---
* Fixed build for GHC 9.4+

2.0.0.0
---
* replaced `Integral` instance with `Fractional` instance (see #8 and #14)
* added a constraint to ensure the type-level modulus is never 0
* made `inv` return `Maybe` instead of raising an error
* misc. refactoring and improvements

1.2.1.3
---
* fixed a name clash with GHC.TypeLits for base >= 4.11.0

1.2.1.2
---
* exported the `/` type operator with `ExplicitNamespaces` enabled to
  support GHC 8. Should be backwards compatible through GHC 7.6.

1.2.1.1
---
* added a basic test suite with doctests

1.2.1.0
---
* changed `Integral` implementation: `quotRem` now uses modular inversion!
* added `inv` for modular inversion
* added `SomeMod` data type for modular number with unknown modulus
* added `modVal` and `someModVal` helpers similar to ones in `GHC.TypeLits`

