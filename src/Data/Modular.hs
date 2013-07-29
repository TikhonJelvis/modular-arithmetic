{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- This module provides types for working with integers modulo some
-- constant.
-- 
-- This module uses some new Haskell features introduced in 7.6. In
-- particular, it needs @DataKinds@ and type literals
-- ("GHC.TypeLits"). The @TypeOperators@ extension is needed for the
-- nice infix syntax.
-- 
-- These types are created with the type constructor 'Mod'
-- (or its synonym '/'). To work with integers mod 7, you could write:
-- 
-- > Int `Mod` 7
-- > Integer `Mod` 7
-- > Integer/7
-- > ℤ/7
-- 
-- (The last is a synonym for @Integer@ provided by this library. In
-- Emacs, you can use the TeX input mode to type it with @\\Bbb{Z}@.)
-- 
-- All the usual typeclasses are defined for these types. You can also
-- get the constant using @bound@ or extract the underlying value
-- using @unMod@.
--
-- Here is a quick example:
-- 
-- > *Data.Modular> (10 :: ℤ/7) * (11 :: ℤ/7)
-- > 5
-- 
-- It also works correctly with negative numeric literals:
-- 
-- > *Data.Modular> (-10 :: ℤ/7) * (11 :: ℤ/7)
-- > 2

module Data.Modular (unMod, toMod, toMod', Mod, (/)(), ℤ) where

import           Control.Arrow (first)

import           Data.Ratio    ((%))

import           GHC.TypeLits

-- | The actual type, wrapping an underlying @Integeral@ type @i@ in a
-- newtype annotated with the bound.
newtype i `Mod` (n :: Nat) = Mod i deriving (Eq, Ord)

-- | Extract the underlying integral value from a modular type.
unMod :: i `Mod` n -> i
unMod (Mod i) = i

-- | A synonym for @Mod@, inspired by the ℤ/n syntax from mathematics.
type (/) = Mod

-- | A synonym for Integer, also inspired by the ℤ/n syntax.
type ℤ   = Integer

-- | Returns the bound of the modular type in the type itself. This
-- breaks the invariant of the type, so it shouldn't be used outside
-- this module.
_bound :: forall n i. (Integral i, SingI n) => i `Mod` n
_bound = Mod . fromInteger $ fromSing (sing :: Sing n)

-- | Wraps the underlying type into the modular type, wrapping as
-- appropriate.
toMod :: forall n i. (Integral i, SingI n) => i -> i `Mod` n
toMod i = Mod $ i `mod` unMod (_bound :: i `Mod` n)

-- | Wraps an integral number to a mod, converting between integral
-- types.
toMod' :: forall n i j. (Integral i, Integral j, SingI n) => i -> j `Mod` n
toMod' = toMod . fromIntegral

instance Show i => Show (i `Mod` n) where show (Mod i) = show i
instance (Read i, Integral i, SingI n) => Read (i `Mod` n)
  where readsPrec prec = map (first toMod) . readsPrec prec

instance (Integral i, SingI n) => Num (i `Mod` n) where
  fromInteger = toMod . fromInteger

  Mod i₁ + Mod i₂ = toMod $ i₁ + i₂
  Mod i₁ * Mod i₂ = toMod $ i₁ * i₂

  abs    (Mod i) = toMod $ abs i
  signum (Mod i) = toMod $ signum i
  negate (Mod i) = toMod $ negate i

instance (Integral i, SingI n) => Enum (i `Mod` n) where
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger . unMod

instance (Integral i, SingI n) => Bounded (i `Mod` n) where
  maxBound = pred _bound
  minBound = 0

instance (Integral i, SingI n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

instance (Integral i, SingI n) => Integral (i `Mod` n) where
  toInteger (Mod i) = toInteger i
  Mod i₁ `quotRem` Mod i₂ = let (q, r) = i₁ `quotRem` i₂ in (toMod q, toMod r)
