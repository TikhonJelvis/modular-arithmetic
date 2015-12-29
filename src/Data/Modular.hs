{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}

-- |
-- Types for working with integers modulo some constant.
module Data.Modular (
  -- $doc

  -- * Preliminaries
  -- $setup

  -- * Modular arithmetic
  Mod,
  unMod, unMod', toMod, toMod', toMod'',
  inv, (/)(), ℤ,
  modVal, SomeMod, someModVal,
  Unsign
) where

import           Control.Arrow (first)

import           Data.Proxy    (Proxy (..))
import           Data.Ratio    ((%))
import           Data.Int
import           Data.Word

import           GHC.TypeLits

import           Numeric.Natural


-- $setup
--
-- To use type level numeric literals you need to enable
-- the @DataKinds@ extension:
--
-- >>> :set -XDataKinds
--
-- To use infix syntax for @'Mod'@ or the @/@ synonym,
-- enable @TypeOperators@:
--
-- >>> :set -XTypeOperators

-- $doc
--
-- @'Mod'@ and its synonym @/@ let you wrap arbitrary numeric types
-- in a modulus. To work with integers (mod 7) backed by @'Integer'@,
-- you could use one of the following equivalent types:
--
-- > Mod Integer 7
-- > Integer `Mod` 7
-- > Integer/7
-- > ℤ/7
--
-- (@'ℤ'@ is a synonym for @'Integer'@ provided by this library. In
-- Emacs, you can use the TeX input mode to type it with @\\Bbb{Z}@.)
--
-- The usual numeric typeclasses are defined for these types. You can
-- always extract the underlying value with @'unMod'@.
--
-- Here is a quick example:
--
-- >>> 10 * 11 :: ℤ/7
-- 5
--
-- It also works correctly with negative numeric literals:
--
-- >>> (-10) * 11 :: ℤ/7
-- 2
--
-- Modular division is an inverse of modular multiplication.
-- It is defined when divisor is coprime to modulus:
--
-- >>> 7 `div` 3 :: ℤ/16
-- 13
-- >>> 3 * 13 :: ℤ/16
-- 7
--
-- To use type level numeric literals you need to enable the
-- @DataKinds@ extension and to use infix syntax for @Mod@ or the @/@
-- synonym, you need @TypeOperators@.

type family Unsign (i :: *) :: * -- = (u :: *) where u -> i

type instance Unsign Int = Word
type instance Unsign Int8 = Word8
type instance Unsign Int16 = Word16
type instance Unsign Int32 = Word32
type instance Unsign Int64 = Word64
type instance Unsign Integer = Natural


-- | Wraps an underlying @Integeral@ type @i@ in a newtype annotated
-- with the bound @n@.
newtype i `Mod` (n :: Nat) = Mod (Unsign i)

deriving instance Eq (Unsign i) => Eq (Mod i n)
deriving instance Ord (Unsign i) => Ord (Mod i n)

-- | Extract the underlying natural value from a modular value.
unMod :: i `Mod` n -> Unsign i
unMod (Mod i) = i

-- | Extract the unquotiented intergral value from a modular value.
unMod' :: (Integral (Unsign i), Num i) => i `Mod` n -> i
unMod' (Mod i) = fromIntegral i


-- | A synonym for @Mod@, inspired by the ℤ/n syntax from mathematics.
type (/) = Mod

-- | A synonym for Integer, also inspired by the ℤ/n syntax.
type ℤ   = Integer

-- | Injects a value of the quotiented type into the modulus type,
-- wrapping as appropriate.
toMod :: forall n i. (Integral i, Num (Unsign i), KnownNat n) => i -> i `Mod` n
toMod i = Mod $ fromIntegral $ i `mod` fromInteger (natVal (Proxy :: Proxy n))

-- | Wraps an integral number, converting between integral types.
toMod' :: forall n i j. (Integral i, Integral j, Num (Unsign j), KnownNat n) => i -> j `Mod` n
toMod' i = toMod . fromIntegral $ i `mod` (fromInteger $ natVal (Proxy :: Proxy n))

-- | Injects a value of the underlying type into the modulus type,
-- wrapping as appropriate.
toMod'' :: forall n i. (Integral (Unsign i), KnownNat n) => Unsign i -> i `Mod` n
toMod'' i = Mod $ fromIntegral $ i `mod` fromInteger (natVal (Proxy :: Proxy n))


instance Show (Unsign i) => Show (i `Mod` n) where
  show (Mod i) = show i

instance (Read (Unsign i), Integral (Unsign i), KnownNat n) => Read (i `Mod` n) where
  readsPrec prec = map (first toMod'') . readsPrec prec

instance (Integral i, Integral (Unsign i), KnownNat n) => Num (i `Mod` n) where
  fromInteger = toMod'' . fromInteger

  Mod i₁ + Mod i₂ = toMod'' $ i₁ + i₂
  Mod i₁ * Mod i₂ = toMod'' $ i₁ * i₂

  abs    (Mod i) = toMod'' $ abs i
  signum (Mod i) = toMod'' $ signum i
  negate i       = toMod $ negate $ unMod' i

instance (Integral i, Integral (Unsign i), KnownNat n) => Enum (i `Mod` n) where
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . toInteger . unMod

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise               = minBound

instance (Integral i, Integral (Unsign i), KnownNat n) => Bounded (i `Mod` n) where
  maxBound = -1
  minBound = 0

instance (Integral i, Integral (Unsign i), KnownNat n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

-- | Integer division uses modular inverse @'inv'@, so it is possible
-- to divide only by numbers coprime to @n@ and the remainder is
-- always @0@.
instance (Integral i, Integral (Unsign i), KnownNat n, Show (Unsign i)) => Integral (i `Mod` n) where
  toInteger (Mod i) = toInteger i
  i₁ `quotRem` i₂ = (i₁ * inv i₂, 0)

-- | The modular inverse.
--
-- >>> inv 3 :: ℤ/7
-- 5
-- >>> 3 * 5 :: ℤ/7
-- 1
--
-- Note that only numbers coprime to @n@ have an inverse modulo @n@:
--
-- >>> inv 6 :: ℤ/15
-- *** Exception: divide by 6 (mod 15), non-coprime to modulus
--
inv :: forall n i. (KnownNat n, Integral (Unsign i), Integral i, Show (Unsign i))
    => Mod i n -> Mod i n
inv divisor = toMod $ snd $ inv' modulus' divisor'
  where
    modulus :: Unsign i
    modulus = fromInteger $ natVal (Proxy :: Proxy n)

    modulus', divisor' :: i
    modulus' = fromIntegral modulus
    divisor' = unMod' divisor

    -- backwards Euclidean algorithm
    inv' :: i -> i -> (i , i)
    inv' _ 0 = error $
      "divide by " ++ show divisor ++ " (mod " ++ show modulus ++ "), non-coprime to modulus"
    inv' _ 1 = (0, 1)
    inv' n x = (r', q' - r' * q)
      where
        (q,  r)  = n `quotRem` x
        (q', r') = inv' x r

-- | A modular number with an unknown bound.
data SomeMod i where
  SomeMod :: forall i (n :: Nat). KnownNat n => Mod i n -> SomeMod i

instance Show (Unsign i) => Show (SomeMod i) where
  showsPrec p (SomeMod x) = showsPrec p x

-- | Convert an integral number @i@ into a @'Mod'@ value given modular
-- bound @n@ at type level.
modVal :: forall i proxy n. (Integral i, Integral (Unsign i), KnownNat n) => i -> proxy n -> Mod i n
modVal i _ = toMod i

-- | Convert an integral number @i@ into a @'Mod'@ value with an
-- unknown modulus.
someModVal :: (Integral i, Integral (Unsign i)) => i -> Integer -> Maybe (SomeMod i)
someModVal i n =
  case someNatVal n of
    Nothing -> Nothing
    Just (SomeNat proxy) -> Just (SomeMod (modVal i proxy))
