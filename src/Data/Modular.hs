{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.Modular where

import           Control.Arrow (first)

import           Data.Ord      (comparing)
import           Data.Ratio    ((%))

import           GHC.TypeLits

newtype i `Mod` (n :: Nat) = Mod { unMod :: i } deriving (Eq, Ord)

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
