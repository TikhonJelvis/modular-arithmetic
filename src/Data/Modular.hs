{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}

{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE CPP#-}
-- |
-- Types for working with integers modulo some constant.
module Data.Modular (
  -- $doc

  -- * Preliminaries
  -- $setup

  -- * Modular arithmetic
  Mod,
  unMod, toMod, toMod',
  inv, type (/)(), ℤ,
  modVal, SomeMod, someModVal
) where

import           Control.Arrow (first)

import           Data.Proxy    (Proxy (..))
import           Data.Ratio    ((%))

#if MIN_VERSION_base(4,11,0)
import           GHC.TypeLits hiding (Mod)
#else
import           GHC.TypeLits
#endif

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

-- | Wraps an underlying @Integeral@ type @i@ in a newtype annotated
-- with the bound @n@.
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
_bound :: forall n i. (Integral i, KnownNat n) => i
_bound = fromInteger $ natVal (Proxy :: Proxy n)
                            
-- | Injects a value of the underlying type into the modulus type,
-- wrapping as appropriate.
toMod :: forall n i. (Integral i, KnownNat n) => i -> i `Mod` n
toMod i = Mod $ i `mod` (_bound @n)

-- | Wraps an integral number, converting between integral types.
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
  toEnum = fromIntegral
  fromEnum = fromIntegral . unMod

  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

instance (Integral i, KnownNat n) => Bounded (i `Mod` n) where
  maxBound = Mod $ pred (_bound @n)
  minBound = 0

instance (Integral i, KnownNat n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

-- | Integer division uses modular inverse @'inv'@, so it is possible
-- to divide only by numbers coprime to @n@ and the remainder is
-- always @0@.
instance (Integral i, KnownNat n) => Integral (i `Mod` n) where
  toInteger (Mod i) = toInteger i
  i₁ `quotRem` i₂ = (i₁ * unwrap (inv i₂), 0)
    where
      -- these are only used for error message
      modulus = show (_bound @n @Integer)
      divisor = show (toInteger i₂)
      unwrap (Just x) = x
      unwrap Nothing  =
        error ("divide by " ++ divisor ++
               " (mod " ++ modulus ++ "), non-coprime to modulus")

-- | The modular inverse.
--
-- >>> inv 3 :: Maybe (ℤ/7)
-- Just 5
-- >>> 3 * 5 :: ℤ/7
-- 1
--
-- Note that only numbers coprime to @n@ have an inverse modulo @n@:
--
-- >>> inv 6 :: Maybe (ℤ/15)
-- Nothing
--
inv :: forall n i. (KnownNat n, Integral i) => (i/n) -> Maybe (i/n)
inv (Mod k) = toMod . snd <$> inv' (_bound @n) k
  where
    -- backwards Euclidean algorithm
    inv' _ 0 = Nothing
    inv' _ 1 = Just (0, 1)
    inv' n x = do
      let (q,  r)  = n `quotRem` x
      (q', r') <- inv' x r
      pure (r', q' - r' * q)

-- | A modular number with an unknown bound.
data SomeMod i where
  SomeMod :: forall i (n :: Nat). KnownNat n => Mod i n -> SomeMod i

instance Show i => Show (SomeMod i) where
  showsPrec p (SomeMod x) = showsPrec p x

-- | Convert an integral number @i@ into a @'Mod'@ value given modular
-- bound @n@ at type level.
modVal :: forall i proxy n. (Integral i, KnownNat n) => i -> proxy n -> Mod i n
modVal i _ = toMod i

-- | Convert an integral number @i@ into a @'Mod'@ value with an
-- unknown modulus.
someModVal :: Integral i => i -> Integer -> Maybe (SomeMod i)
someModVal i n =
  case someNatVal n of
    Nothing -> Nothing
    Just (SomeNat proxy) -> Just (SomeMod (modVal i proxy))
