{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RoleAnnotations #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Rounded
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2017 Claude Heiland-Allen
--                (C) 2017 Michal Konecny
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.RoundedSimple
(
-- * Floating point numbers with a specified rounding mode and precision
  Rounded
-- , fromInt
-- , fromDouble
, toString
-- * operations analogous to RealFloat
, onRounded_floatRadix
, onRounded_floatDigits
, onRounded_decodeFloat
, onRounded_encodeFloat
, onRounded_isNaN
, onRounded_isInfinite
, onRounded_isDenormalized
, onRounded_isNegativeZero
, getExp
-- * Precision and Rounding
, module Numeric.RoundedSimple.RoundPrec
, set
, getPrec
-- * Conversions
, toDouble
, toRational'
, fromRational'
, toInteger'
, fromInteger'
, onRounded_properFraction
, onRounded_round
-- * Useful Constants
, kPi
, kLog2
, kEuler
, kCatalan

-- * Combinators that are oblivious to precision
-- , succUlp
-- , predUlp
-- * Mixed-precision operations
, add_
, sub_
, mul_
, div_
, abs_
, negate_
, compare_
, min_
, max_
, modf
, sqrt_
, exp_
, log_
, sin_
, tan_
, cos_
, asin_
, atan_
, atan2_
, acos_
, sinh_
, tanh_
, cosh_
, asinh_
, atanh_
, acosh_
, log1p_
, expm1_

, in_'
, out_''
)
where

-- import Control.Exception (bracket, bracket_, throwIO, ArithException(Overflow))
import Control.Exception (bracket, throwIO, ArithException(Overflow))
import Data.Bits (shiftL, testBit)
-- import Data.Int (Int32)
import Data.Ratio ((%))

-- import Foreign (with, alloca, allocaBytes, peek, sizeOf, nullPtr)
import Foreign (with, allocaBytes, peek, sizeOf, nullPtr)
import Foreign.C (CInt(..), CIntMax(..)) --, CSize(..), CChar(..))
import Foreign.C.String (peekCString)

import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim
  ( ByteArray#
  , sizeofByteArray#
  , copyByteArrayToAddr#
  , newByteArray#
  , copyAddrToByteArray#
  , unsafeFreezeByteArray#
  )
import GHC.Types (IO(..))
import GHC.Exts (Ptr(..), Int(..))

#if MIN_VERSION_base(4,9,0)
import Numeric (Floating(..))
#endif
-- import Numeric (readSigned, readFloat)

import Numeric.GMP.Utils (withInInteger, withOutInteger, withOutInteger_, withInRational)
-- import Numeric.GMP.Types (MPZ, MPQ, MPLimb)
import Numeric.GMP.Types (MPLimb)

import Numeric.MPFR.Types
import Numeric.MPFR.Functions

import Numeric.RoundedSimple.RoundPrec

-- | A properly rounded floating-point number with a given rounding mode and precision.
--
-- You can 'Data.Coerce.coerce' to change rounding modes, but not precision.
data Rounded = Rounded
  { roundedPrec  :: !MPFRPrec
  , _roundedSign  :: !MPFRSign
  , roundedExp   :: !MPFRExp
  , _roundedLimbs :: !ByteArray#
  }

getPrec :: Rounded -> Precision
getPrec = fromIntegral . roundedPrec

cmp' :: Comparison -> Rounded -> Rounded -> CInt
cmp' f a b = unsafePerformIO $
  in_ a $ \afr ->
  in_ b $ \bfr -> do
  f afr bfr

cmp :: Comparison -> Rounded -> Rounded -> Bool
cmp f a b = cmp' f a b /= 0

compare_ :: Rounded -> Rounded -> Ordering
compare_ a b = compare (cmp' mpfr_cmp a b) 0

instance Eq Rounded where
  (==) = cmp mpfr_equal_p
  (/=) = cmp mpfr_lessgreater_p

instance Ord Rounded where
  compare a b = compare (cmp' mpfr_cmp a b) 0
  (<=) = cmp mpfr_lessequal_p
  (>=) = cmp mpfr_greaterequal_p
  (<) = cmp mpfr_less_p
  (>) = cmp mpfr_greater_p
  min = binaryExact mpfr_min
  max = binaryExact mpfr_max

{-| Returns the value of op with given Precision and rounded with RoundingMode. -}
set :: RoundingMode -> Precision -> Rounded -> Rounded
set = unary mpfr_set

sgn :: Rounded -> Ordering
sgn x = compare (unsafePerformIO $ in_ x mpfr_sgn) 0

tst :: Test -> Rounded -> Bool
tst f x = unsafePerformIO $ in_ x $ \xfr -> do
  t <- f xfr
  return (t /= 0)

decodeFloat' :: Rounded -> (Integer, Int)
decodeFloat' x = case (unsafePerformIO $ do
  in_ x $ \xfr -> withOutInteger $ \xz -> with 0 $ \flagsptr -> do
    e <- wrapped_mpfr_get_z_2exp xz xfr flagsptr
    flags <- peek flagsptr
    case testBit flags erangeBit of
      False -> return (fromIntegral e)
      True -> throwIO Overflow) of
  (0, _) -> (0, 0) -- mpfr_get_z_2exp returns emin instead of 0 for exponent
  me -> me

erangeBit :: Int
erangeBit = 5 -- sync with cbits/wrappers.c

encodeFloat' :: Precision -> Integer -> Int -> Rounded
encodeFloat' p j e = r where
  r = unsafePerformIO $ do
        (Just x, _) <- withInInteger j $ \jz -> out_ p $ \xfr -> mpfr_set_z_2exp xfr jz (fromIntegral e) (rnd TowardNearest)
        return x

-- functions analogous to methods of RealFloat:
onRounded_floatRadix :: Rounded -> Integer
onRounded_floatRadix  _ = 2
onRounded_floatDigits :: Rounded -> Int
onRounded_floatDigits = getPrec

--   -- FIXME: this should do for now, but the real ones can change...
-- mpfr_floatRange _ = (fromIntegral (minBound :: Int32), fromIntegral (maxBound :: Int32))

onRounded_decodeFloat :: Rounded -> (Integer, Int)
onRounded_decodeFloat = decodeFloat'
onRounded_encodeFloat :: Precision -> Integer -> Int -> Rounded
onRounded_encodeFloat = encodeFloat'
onRounded_isNaN :: Rounded -> Bool
onRounded_isNaN = tst mpfr_nan_p
onRounded_isInfinite :: Rounded -> Bool
onRounded_isInfinite = tst mpfr_inf_p
onRounded_isDenormalized :: Rounded -> Bool
onRounded_isDenormalized _ = False
onRounded_isNegativeZero :: Rounded -> Bool
onRounded_isNegativeZero r = tst mpfr_zero_p r && tst mpfr_signbit r
-- mpfr_isIEEE _ = True -- is this a lie? it mostly behaves like an IEEE float, despite being much bigger
-- mpfr_atan2 = atan2_

getExp :: Rounded -> Int
getExp = fromIntegral . roundedExp

instance Show Rounded where
  show x = toString 30 x

toString :: Int -> Rounded -> String
-- FIXME: what do about unsightly 0.1 -> 0.1000...0002 or 9.999...9995e-2 issues
toString maxN x = unsafePerformIO $ do
  (s, e) <- in_ x $ \xfr -> with 0 $ \eptr -> do
    s <- bracket (mpfr_get_str nullPtr eptr 10 0 xfr (rnd TowardNearest)) mpfr_free_str peekCString
    e <- peek eptr
    return (s, fromIntegral e)
  return $ case () of
    _ | onRounded_isNaN x -> "NaN"
      | onRounded_isInfinite x && sgn' == GT -> "Infinity"
      | onRounded_isInfinite x -> "-Infinity"
      | onRounded_isNegativeZero x -> "-0.0"
      | sgn' == EQ -> "0.0"
      | e <  0 ||
        e >= threshold -> sign ++ take 1 digits  ++ "." ++
                          dropTrailingZeroes (take (n - 1) (drop 1 digits0)) ++
                          "e" ++ show (e - 1)
      | e == 0         -> sign ++ "0." ++
                          (take n (dropTrailingZeroes digits))
      | e <  threshold -> sign ++ take e digits0 ++ "." ++
                          dropTrailingZeroes (take (n - e) (drop e digits0))
      | otherwise -> error "internal error in toString"
      where
        sgn' = sgn x
        sign = case sgn' of
          GT -> ""
          EQ -> ""
          LT -> "-"
        threshold = 8
        n = min maxN $ length digits
        digits = case take 1 s of
          "-" -> drop 1 s
          _ -> s
        digits0 = digits ++ repeat '0'
        dropTrailingZeroes a = case dropWhile ('0' ==) (reverse a) of
          "" -> "0"
          b -> reverse b




kPi, kLog2, kEuler, kCatalan :: RoundingMode -> Precision -> Rounded
kPi = constant mpfr_const_pi
kLog2 = constant mpfr_const_log2
kEuler = constant mpfr_const_euler
kCatalan = constant mpfr_const_catalan

abs_, negate_, log_, exp_, sqrt_,
 sin_, cos_, tan_, asin_, acos_, atan_,
   sinh_, cosh_, tanh_, asinh_, acosh_, atanh_,
     log1p_, expm1_
  :: RoundingMode -> Precision -> Rounded -> Rounded
abs_ = unary mpfr_abs
negate_ = unary mpfr_neg
log_ = unary mpfr_log
exp_ = unary mpfr_exp
sqrt_ = unary mpfr_sqrt
sin_ = unary mpfr_sin
cos_ = unary mpfr_cos
tan_ = unary mpfr_tan
asin_ = unary mpfr_asin
acos_ = unary mpfr_acos
atan_ = unary mpfr_atan
sinh_ = unary mpfr_sinh
cosh_ = unary mpfr_cosh
tanh_ = unary mpfr_tanh
asinh_ = unary mpfr_asinh
acosh_ = unary mpfr_acosh
atanh_ = unary mpfr_atanh
log1p_ = unary mpfr_log1p
expm1_ = unary mpfr_expm1

min_, max_, add_, sub_, mul_, div_, atan2_
  :: RoundingMode -> Precision -> Rounded -> Rounded -> Rounded
min_ = binary mpfr_min
max_ = binary mpfr_max
add_ = binary mpfr_add
sub_ = binary mpfr_sub
mul_ = binary mpfr_mul
div_ = binary mpfr_div
atan2_ = binary mpfr_atan2

toRational' :: Rounded -> Rational
toRational' r
   | e > 0     = fromIntegral (s `shiftL` e)
   | otherwise = s % (1 `shiftL` negate e)
   where (s, e) = decodeFloat' r

fromRational' :: RoundingMode -> Precision -> Rational -> Rounded
fromRational' r p q = result where -- TODO small integer optimisation
  result = unsafePerformIO $ do
        (Just x, _) <- withInRational q $ \qq -> out_ p $ \qfr -> mpfr_set_q qfr qq (rnd r)
        return x

modf :: RoundingMode -> Precision -> Rounded -> (Rounded, Rounded)
modf r p x = unsafePerformIO $ do
  (Just y, (Just z, _)) <- in_ x $ \xfr ->
    out_ p $ \yfr ->
      out_ p $ \zfr ->
        mpfr_modf yfr zfr xfr (rnd r)
  return (y, z)

-- | Round to 'Double' with the given rounding mode.
toDouble :: RoundingMode -> Rounded -> Double
toDouble r x = unsafePerformIO $ in_ x $ \xfr -> mpfr_get_d xfr (rnd r)
{-# RULES "realToFrac/toDouble" forall (x :: Rounded) . realToFrac x = toDouble TowardNearest x #-}

toInteger' :: RoundingMode -> Rounded -> Integer
toInteger' r x = unsafePerformIO $
  withOutInteger_ $ \yz ->
    in_ x $ \xfr ->
      with 0 $ \flagsptr -> do
        e <- wrapped_mpfr_get_z yz xfr (rnd r) flagsptr
        flags <- peek flagsptr
        case testBit flags erangeBit of
          False -> return e
          True -> throwIO Overflow

fromInteger' :: RoundingMode -> Precision -> Integer -> Rounded
fromInteger' r p j = result where
  result = unsafePerformIO $ do
        if toInteger (minBound :: CIntMax) <= j && j <= toInteger (maxBound :: CIntMax)
        then do
          (Just x, _) <- out_ p $ \jfr -> mpfr_set_sj jfr (fromInteger j :: CIntMax) (rnd r)
          return x
        else do
          (Just x, _) <- withInInteger j $ \jz -> out_ p $ \jfr -> mpfr_set_z jfr jz (rnd r)
          return x

onRounded_properFraction :: (Integral a) => RoundingMode -> Precision -> Rounded -> (a, Rounded)
onRounded_properFraction r p x = (fromInteger (toInteger' r i), f) where
  (i, f) = modf r p x
-- this round is from base-4.9.1.0, modified to use compare instead of signum
onRounded_round :: (Integral a) => RoundingMode -> Precision -> Rounded -> a
onRounded_round r p x =
          let (n,f) = onRounded_properFraction r p x
              m     = if tst mpfr_signbit f then n - 1 else n + 1
          in  case compare_ (abs_ r p x) (fromRational' Up 10 0.5) of
                LT -> n
                EQ -> if even n then n else m
                GT -> m

------------------------------------------------------------

constant :: Constant -> RoundingMode -> Precision -> Rounded
constant k r p = result where
  result = unsafePerformIO $ do
    (Just x, _) <- out_ p $ \xfr -> k xfr (rnd r)
    return x

unary :: Unary -> RoundingMode -> Precision -> Rounded -> Rounded
unary f r p a = unsafePerformIO $ do
  (Just c, _) <- in_ a $ \afr ->
    out_ p $ \cfr ->
      f cfr afr (rnd r)
  return c

binary :: Binary -> RoundingMode -> Precision -> Rounded -> Rounded -> Rounded
binary f r p a b = unsafePerformIO $ do
  (Just c, _) <-
    in_ a $ \afr ->
      in_ b $ \bfr ->
        out_ p $ \cfr ->
          f cfr afr bfr (rnd r)
  return c

binaryExact :: Binary -> Rounded -> Rounded -> Rounded
binaryExact f x y = binary f TowardNearest p x y
  where
  p = getPrec x `max` getPrec y

in_' :: Rounded -> (MPFR -> IO a) -> IO a
in_' (Rounded p s e l) f = withByteArray l $ \ptr _bytes -> f MPFR
  { mpfrPrec = p
  , mpfrSign = s
  , mpfrExp = e
  , mpfrD = ptr
  }

in_ :: Rounded -> (Ptr MPFR -> IO a) -> IO a
in_ x f = in_' x $ \y -> with y f

out_'' :: MPFR -> IO Rounded
out_'' MPFR { mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } =
  asByteArray d' (precBytes p') $ \l' -> return (Rounded p' s' e' l')

out_' :: MPFRPrec -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded), a)
out_' p f = allocaBytes (precBytes p) $ \d -> with
  MPFR{ mpfrPrec = p, mpfrSign = 0, mpfrExp = 0, mpfrD = d } $ \ptr -> do
  a <- f ptr
  mpfr' <- peek ptr
  if p /= mpfrPrec mpfr' then return (Nothing, a) else
    do
    r' <- out_'' mpfr'
    return (Just r', a)

out_ :: Precision -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded), a)
out_ p f = r where
  r = out_' (prec p) f

-- -- | Use a value as a /constant/ @mpfr_t@ (attempts to modify it may explode,
-- --   changing the precision will explode).
-- withInRounded :: Rounded r p -> (Ptr MPFR -> IO a) -> IO a
-- withInRounded = in_
--
-- -- | Allocates and initializes a new @mpfr_t@, if the precision matches after
-- --   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
-- withOutRounded :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
-- withOutRounded f = r where
--   r = alloca $ \ptr -> bracket_ (mpfr_init2 ptr prec) (mpfr_clear ptr) $ do
--     a <- f ptr
--     MPFR{ mpfrPrec = prec', mpfrSign = s, mpfrExp = e, mpfrD = d } <- peek ptr
--     if prec /= prec'
--       then return (Nothing, a)
--       else asByteArray d (precBytes prec) $ \l ->
--         return (Just (Rounded prec s e l), a)
--   prec = fromIntegral (precision (t r))
--   t :: IO (Maybe b, a) -> b
--   t _ = undefined
--
-- -- | Allocates and initializes a new @mpfr_t@, if the precision matches after
-- --   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
-- --   The result of the action is ignored.
-- withOutRounded_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
-- withOutRounded_ = fmap fst . withOutRounded
--
-- -- | Like 'withOutRounded' but with the limbs allocated by GHC, which should be
-- --   slightly faster.  However, it will crash if MPFR tries to reallocate the
-- --   limbs, so the action must not try to change the precision or clear it, etc.
-- unsafeWithOutRounded :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
-- unsafeWithOutRounded = out_
--
-- -- | Like 'withOutRounded_' but with the limbs allocated by GHC, which should be
-- --   slightly faster.  However, it will crash if MPFR tries to reallocate the
-- --   limbs, so the action must not try to change the precision or clear it, etc.
-- unsafeWithOutRounded_ :: Precision p => (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
-- unsafeWithOutRounded_ = fmap fst . out_
--
-- -- | Allocates and initializes a new @mpfr_t@ to the value.  If the precision matches after
-- --   the action then it is peeked and returned.  Otherwise you get 'Nothing'.
-- withInOutRounded :: Precision p => Rounded r p -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p), a)
-- -- FIXME: optimize to reduce copying
-- withInOutRounded i f =
--   withOutRounded $ \ofr ->
--     in_ i $ \ifr -> do
--       _ <- mpfr_set ofr ifr (fromIntegral (fromEnum TowardNearest))
--       f ofr
--
-- -- | Allocates and initializes a new @mpfr_t@ to the value.  If the precision matches after
-- --   the action then it is peeked and returned.  Otherwise you get 'Nothing'.  The result
-- --   ot the action is ignored.
-- withInOutRounded_ :: Precision p => Rounded r p -> (Ptr MPFR -> IO a) -> IO (Maybe (Rounded r p))
-- withInOutRounded_ x = fmap fst . withInOutRounded x
--
-- -- | Peek an @mpfr_t@ at its actual precision, reified.
-- peekRounded :: Rounding r => Ptr MPFR -> (forall (p :: *) . Precision p => Rounded r p -> IO a) -> IO a
-- peekRounded ptr f = do
--   MPFR{ mpfrPrec = p', mpfrSign = s', mpfrExp = e', mpfrD = d' } <- peek ptr
--   asByteArray d' (precBytes p') $ \l' -> reifyPrecision (fromIntegral p') (wrap f (Rounded p' s' e' l'))
--   where
--     wrap :: forall (p :: *) (r :: RoundingMode) (a :: *) . (Rounding r, Precision p) => (forall (q :: *) . Precision q => Rounded r q -> IO a) -> Rounded r p -> Proxy p -> IO a
--     wrap g r = \_proxy -> g r


-- "The number of limbs in use is controlled by _mpfr_prec, namely ceil(_mpfr_prec/mp_bits_per_limb)."
-- <http://www.mpfr.org/mpfr-current/mpfr.html#Internals>
precBytes :: MPFRPrec -> Int
precBytes p = bytesPerLimb * ((fromIntegral p + bitsPerLimb1) `div` bitsPerLimb)
bytesPerLimb :: Int
bytesPerLimb = sizeOf (undefined :: MPLimb)
bitsPerLimb :: Int
bitsPerLimb = bytesPerLimb * 8
bitsPerLimb1 :: Int
bitsPerLimb1 = bitsPerLimb - 1

withByteArray :: ByteArray# -> (Ptr a -> Int -> IO r) -> IO r
withByteArray ba# f = do
  let bytes = I# (sizeofByteArray# ba#)
  allocaBytes bytes $ \ptr@(Ptr addr#) -> do
    IO (\s -> (# copyByteArrayToAddr# ba# 0# addr# (sizeofByteArray# ba#) s, () #))
    f ptr bytes

asByteArray :: Ptr a -> Int -> (ByteArray# -> IO r) -> IO r
asByteArray (Ptr addr#) (I# bytes#) f = do
  IO $ \s# -> case newByteArray# bytes# s# of
    (# s'#, mba# #) ->
      case unsafeFreezeByteArray# mba# (copyAddrToByteArray# addr# mba# 0# bytes# s'#) of
        (# s''#, ba# #) -> case f ba# of IO r -> r s''#
