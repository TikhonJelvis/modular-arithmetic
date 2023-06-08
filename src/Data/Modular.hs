{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Types for working with integers modulo some constant.
module Data.Modular (
  -- $doc

  -- * Preliminaries
  -- $setup

  -- * Modular arithmetic
  Mod, Modulus,
  unMod, toMod, toMod',
  inv, type (/)(), ℤ,
  modVal, SomeMod, someModVal
) where

import           Control.Arrow          (first)

import           Data.Proxy             (Proxy (..))
import           Data.Ratio             (denominator, numerator, (%))

import           Text.Printf            (printf)

#if MIN_VERSION_base(4,11,0)
import           GHC.TypeLits           hiding (Mod)
#else
import           GHC.TypeLits
#endif

#if !MIN_VERSION_base(4,16,0)
import           Data.Type.Equality     ((:~:) (..))

import           GHC.TypeLits.Compare   ((%<=?), (:<=?) (LE, NLE))
import           GHC.TypeLits.Witnesses (SNat (SNat))
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
--
-- To use type applications with @'toMod'@ and friends:
--
-- >>> :set -XTypeApplications
--

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
-- >>> 7 / 3 :: ℤ/16
-- 13
-- >>> 3 * 13 :: ℤ/16
-- 7
--
-- Note that it raises an exception if the divisor is *not* coprime to
-- the modulus:
--
-- >>> 7 / 4 :: ℤ/16
-- *** Exception: Cannot invert 4 (mod 16): not coprime to modulus.
-- ...
--
-- To use type level numeric literals you need to enable the
-- @DataKinds@ extension and to use infix syntax for @Mod@ or the @/@
-- synonym, you need @TypeOperators@.

-- | Wraps an underlying @Integeral@ type @i@ in a newtype annotated
-- with the bound @n@.
newtype i `Mod` (n :: Nat) = Mod i deriving (Eq, Ord)

-- | Extract the underlying integral value from a modular type.
--
-- >>> unMod (10 :: ℤ/4)
-- 2
unMod :: i `Mod` n -> i
unMod (Mod i) = i

-- | A synonym for @Mod@, inspired by the ℤ/n syntax from mathematics.
type (/) = Mod

-- | A synonym for Integer, also inspired by the ℤ/n syntax.
type ℤ   = Integer

-- | The modulus has to be a non-zero type-level natural number.
type Modulus n = (KnownNat n, 1 <= n)

-- | Helper function to get the modulus of a @ℤ/n@ as a value. Used
-- with type applications:
--
-- >>> modulus @5
-- 5
--
modulus :: forall n i. (Integral i, Modulus n) => i
modulus = fromInteger $ natVal (Proxy :: Proxy n)

-- | Injects a value of the underlying type into the modulus type,
-- wrapping as appropriate.
--
-- If @n@ is ambiguous, you can specify it with @TypeApplications@:
--
-- >>> toMod @6 10
-- 4
--
-- Note that @n@ cannot be 0.
toMod :: forall n i. (Integral i, Modulus n) => i -> i `Mod` n
toMod i = Mod $ i `mod` (modulus @n)

-- | Convert an integral number @i@ into a @'Mod'@ value with the
-- type-level modulus @n@ specified with a proxy argument.
--
-- This lets you use 'toMod' without @TypeApplications@ in contexts
-- where @n@ is ambiguous.
modVal :: forall i proxy n. (Integral i, Modulus n)
       => i
       -> proxy n
       -> Mod i n
modVal i _ = toMod i

-- | Wraps an integral number, converting between integral types.
toMod' :: forall n i j. (Integral i, Integral j, Modulus n)
       => i
       -> j `Mod` n
toMod' i = toMod . fromIntegral $ i `mod` (modulus @n)

instance Show i => Show (i `Mod` n) where show (Mod i) = show i
instance (Read i, Integral i, Modulus n) => Read (i `Mod` n)
  where readsPrec prec = map (first toMod) . readsPrec prec

instance (Integral i, Modulus n) => Num (i `Mod` n) where
  fromInteger = toMod . fromInteger

  Mod i₁ + Mod i₂ = toMod $ i₁ + i₂
  Mod i₁ * Mod i₂ = toMod $ i₁ * i₂

  abs    (Mod i) = toMod $ abs i
  signum (Mod i) = toMod $ signum i
  negate (Mod i) = toMod $ negate i

instance (Integral i, Modulus n) => Enum (i `Mod` n) where
  toEnum = fromIntegral
  fromEnum = fromIntegral . unMod

  -- implementation straight from the report
  enumFrom     x   = enumFromTo     x maxBound
  enumFromThen x y = enumFromThenTo x y bound
    where
      bound | fromEnum y >= fromEnum x = maxBound
            | otherwise                = minBound

instance (Integral i, Modulus n) => Bounded (i `Mod` n) where
  maxBound = Mod $ pred (modulus @n)
  minBound = 0

instance (Integral i, Modulus n) => Real (i `Mod` n) where
  toRational (Mod i) = toInteger i % 1

-- | Division uses modular inverse 'inv' so it is only possible to
-- divide by numbers coprime to @n@.
--
-- >>> 1 / 3 :: ℤ/7
-- 5
-- >>> 3 * 5 :: ℤ/7
-- 1
--
-- >>> 2 / 5 :: ℤ/7
-- 6
-- >>> 5 * 6 :: ℤ/7
-- 2
--
-- Dividing by a number that is not coprime to @n@ will raise an
-- error. Use 'inv' directly if you want to avoid this.
--
-- >>> 2 / 7 :: ℤ/7
-- *** Exception: Cannot invert 0 (mod 7): not coprime to modulus.
-- ...
--
instance (Integral i, Modulus n) => Fractional (i `Mod` n) where
  fromRational r =
    fromInteger (numerator r) / fromInteger (denominator r)
  recip i = unwrap $ inv i
    where
      unwrap (Just x) = x
      unwrap Nothing  =
        let i'     = toInteger $ unMod i
            bound' = modulus @n @Integer
        in error $
             printf "Cannot invert %d (mod %d): not coprime to modulus." i' bound'

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
inv :: forall n i. (Modulus n, Integral i) => (i/n) -> Maybe (i/n)
inv (Mod k) = toMod . snd <$> inv' (modulus @n) k
  where
    -- backwards Euclidean algorithm
    inv' _ 0 = Nothing
    inv' _ 1 = Just (0, 1)
    inv' n x = do
      let (q,  r)  = n `quotRem` x
      (q', r') <- inv' x r
      pure (r', q' - r' * q)

-- | A modular number with an unknown modulus.
--
-- Conceptually @SomeMod i = ∃n. i/n@.
data SomeMod i where
  SomeMod :: forall i (n :: Nat). Modulus n => Mod i n -> SomeMod i

-- | Shows both the number *and* its modulus:
--
-- >>> show (someModVal 10 4)
-- "Just (someModVal 2 4)"
--
-- This doesn't *quite* follow the rule that the show instance should
-- be a Haskell expression that evaluates to the given
-- value—'someModVal' returns a 'Maybe'—but this seems like the
-- closest we can reasonably get.
instance Show i => Show (SomeMod i) where
  showsPrec p (SomeMod (x :: i/n)) = showParen (p > 10) $
    showString $ printf "someModVal %s %d" (show x) (modulus @n @Integer)

-- | Convert an integral number @i@ into @'SomeMod'@ with the modulus
-- given at runtime.
--
-- That is, given @i :: ℤ@, @someModVal i modulus@ is equivalent to @i ::
-- ℤ/modulus@ except we don't know @modulus@ statically.
--
-- >>> someModVal 10 4
-- Just (someModVal 2 4)
--
-- Will return 'Nothing' if the modulus is 0 or negative:
--
-- >>> someModVal 10 (-10)
-- Nothing
--
-- >>> someModVal 10 0
-- Nothing
--
someModVal :: Integral i
           => i
           -- ^ Underlying integer @i@
           -> Integer
           -- ^ Modulus @n@
           -> Maybe (SomeMod i)
someModVal i n = do
  SomeNat (_ :: Proxy n) <- someNatVal n
#if MIN_VERSION_base(4,16,0)
  case Proxy @1 `cmpNat` Proxy @n of
    LTI -> pure $ SomeMod $ toMod @n i
    EQI -> pure $ SomeMod $ toMod @n i
    GTI -> Nothing
#else
  case SNat @1 %<=? SNat @n of
    LE Refl -> pure $ SomeMod $ toMod @n i
    NLE _ _ -> Nothing
#endif
