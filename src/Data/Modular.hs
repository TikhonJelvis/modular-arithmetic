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

module Data.Modular (unMod, toMod, toMod', Mod, inv, (/)(), ℤ) where

import           Control.Arrow (first)

import           Data.Proxy    (Proxy (..))
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
_bound :: forall n i. (Integral i, KnownNat n) => i `Mod` n
_bound = Mod . fromInteger $ natVal (Proxy :: Proxy n)
                            
-- | Wraps the underlying type into the modular type, wrapping as
-- appropriate.
toMod :: forall n i. (Integral i, KnownNat n) => i -> i `Mod` n
toMod i = Mod $ i `mod` unMod (_bound :: i `Mod` n)

-- | Wraps an integral number to a mod, converting between integral
-- types.
toMod' :: forall n i j. (Integral i, Integral j, KnownNat n) => i -> j `Mod` n
toMod' i = toMod . fromIntegral $ i `mod` (fromInteger $ natVal (Proxy :: Proxy n))

instance Show i => Show (i `Mod` n) where show (Mod i) = show i
instance (Read i, Integral i, KnownNat n) => Read (i `Mod` n)
  where readsPrec prec = map (first toMod) . readsPrec prec

instance (Integral i, KnownNat n) => Num (i `Mod` n) where
  fromInteger = toMod . fromInteger

  Mod i₁ + Mod i₂ = toMod $ i₁ + i₂
  Mod i₁ * Mod i₂ = toMod $ i₁ * i₂

  abs    (Mod i) = toMod $ abs i
  signum (Mod i) = toMod $ signum i
  negate (Mod i) = toMod $ negate i

instance (Integral i, KnownNat n) => Enum (i `Mod` n) where
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger . unMod

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise               = minBound

instance (Integral i, KnownNat n) => Bounded (i `Mod` n) where
  maxBound = pred _bound
  minBound = 0

instance (Integral i, KnownNat n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

-- | Integer division uses modular inverse @'inv'@,
-- so it is possible to divide only by numbers coprime to @n@
-- and the remainder is always @0@.
instance (Integral i, KnownNat n) => Integral (i `Mod` n) where
  toInteger (Mod i) = toInteger i
  i₁ `quotRem` i₂ = (i₁ * inv i₂, 0)

-- | The modular inverse.
-- Note that only numbers coprime to @n@ have an inverse modulo @n@.
inv :: forall n i. (KnownNat n, Integral i) => Mod i n -> Mod i n
inv = toMod . snd . inv' (fromInteger (natVal (Proxy :: Proxy n))) . unMod
  where
    inv' _ 1 = (0, 1)
    inv' n x = (r', q' - r' * q)
      where
        (q,  r)  = n `quotRem` x
        (q', r') = inv' x r

