{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- This module provides types for working with integers modulo some
-- constant.
-- 
-- This module uses some new Haskell features introduced in 7.6. In
-- particular, it needs DataKinds and type literals
-- (GHC.TypeLits). The TypeOperators extension is needed for the nice
-- infix syntax.
-- 
-- These types are created with the type constructor @Mod@
-- (or its synonym @/@). To work with integers mod 7, you could write:
-- @
-- Int `Mod` 7
-- Integer `Mod` 7
-- Integer/7
-- ℤ/7
-- @
-- 
-- (The last is a synonym for @Integer@ provided by this library. In
-- Emacs, you can use the Tex input mode to type it with \Bbb{Z}.)
-- 
-- All the usual typeclasses are defined for these types. You can also
-- get the constant using @bound@ or extract the underlying value
-- using @unMod@.
--
-- Here is a quick example:
-- @
-- *Data.Modular> (10 :: ℤ/7) * (11 :: ℤ/7)
-- 5
-- @
-- 
-- It also works correctly with negative numeric literals:
-- @
-- *Data.Modular> (-10 :: ℤ/7) * (11 :: ℤ/7)
-- 2
-- @

module Data.Modular (unMod, Mod, (/), ℤ) where

import           Control.Arrow (first)

import           Data.Ord      (comparing)
import           Data.Ratio    ((%))

import           GHC.TypeLits

newtype i `Mod` (n :: Nat) = Mod i deriving (Eq, Ord)

unMod :: i `Mod` n -> i
unMod (Mod i) = i

type (/) = Mod
type ℤ   = Integer

bound :: forall n i. (Integral i, SingI n) => i `Mod` n
bound = Mod . fromInteger $ fromSing (sing :: Sing n)

toMod :: forall n i. (Integral i, SingI n) => i -> i `Mod` n
toMod i = Mod $ i `mod` unMod (bound :: i `Mod` n)

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
  maxBound = pred bound
  minBound = 0

instance (Integral i, SingI n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

instance (Integral i, SingI n) => Integral (i `Mod` n) where
  toInteger (Mod i) = toInteger i
  Mod i₁ `quotRem` Mod i₂ = let (q, r) = i₁ `quotRem` i₂ in (toMod q, toMod r)
