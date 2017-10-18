{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.RoundedSimple.RoundPrec
-- Copyright   :  (C) 2012-2014 Edward Kmett, Daniel Peebles
--                (C) 2013-2017 Claude Heiland-Allen
-- License     :  LGPL
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Numeric.RoundedSimple.RoundPrec
where

import Data.Data

import Numeric.MPFR.Types

data RoundingMode
  = TowardNearestWithTiesAwayFromZero -- ^ currently unsupported placeholder
  | TowardNearest -- ^ roundTiesToEven in IEEE 754-2008
  | TowardZero    -- ^ roundTowardZero in IEEE 754-2008
  | TowardInf     -- ^ roundTowardPositive in IEEE 754-2008
  | Up -- ^ synonym of TowardInf
  | TowardNegInf  -- ^ roundTowardNegative in IEEE 754-2008
  | Down -- ^ synonym of TowardNegInf
  | AwayFromZero  -- ^ round away from zero
  | Faithfully    -- ^ currently unsupported placeholder
  deriving (Eq,Ord,Show,Read,Data,Typeable)


instance Enum RoundingMode where
  toEnum (-1) = TowardNearestWithTiesAwayFromZero
  toEnum 0 = TowardNearest
  toEnum 1 = TowardZero
  toEnum 2 = TowardInf
  toEnum 3 = TowardNegInf
  toEnum 4 = AwayFromZero
  toEnum 5 = Faithfully
  toEnum _ = error "out of range"

  fromEnum TowardNearestWithTiesAwayFromZero = -1
  fromEnum TowardNearest = 0
  fromEnum TowardZero = 1
  fromEnum TowardInf = 2
  fromEnum Up = 2
  fromEnum TowardNegInf = 3
  fromEnum Down = 3
  fromEnum AwayFromZero = 4
  fromEnum Faithfully = 5

rnd :: RoundingMode -> MPFRRnd
rnd = fromIntegral . fromEnum

-- instance Bounded RoundingMode where
--   minBound = TowardNearestWithTiesAwayFromZero
--   maxBound = Faithfully

type Precision = Int

prec :: Precision -> MPFRPrec
prec = fromIntegral
