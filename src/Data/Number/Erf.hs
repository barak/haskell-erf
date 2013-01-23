{-# LANGUAGE ForeignFunctionInterface #-}
module Data.Number.Erf(Erf(..)) where
import Foreign.C

foreign import ccall "erf" c_erf :: CDouble -> CDouble
foreign import ccall "erfc" c_erfc :: CDouble -> CDouble
foreign import ccall "erff" c_erff :: CFloat -> CFloat
foreign import ccall "erfcf" c_erfcf :: CFloat -> CFloat

-- |Error function related functions.
--
-- The derivative of 'erf' is @\ x -> 2 / sqrt pi * exp (x^2)@,
-- and this uniquely determines 'erf' by @erf 0 = 0@.
--
-- Minimal complete definition is 'erfc' or 'normcdf'.
class (Floating a) => Erf a where
    erf :: a -> a
    erfc :: a -> a       -- ^@erfc x = 1 - erf x@
    erfcx :: a -> a      -- ^@erfcx x = exp (x*x) * erfc x@
    normcdf :: a -> a    -- ^@normcdf x = erfc(-x / sqrt 2) / 2@

    -- All the functions are inter-related, here's some defaults.
    erf x = 1 - erfc x
    erfc x = 2 * normcdf (-x * sqrt 2)
    erfcx x = exp (x*x) * erfc x
    normcdf x = erfc(-x / sqrt 2) / 2

instance Erf Double where
    erf = realToFrac . c_erf . realToFrac
    erfc = realToFrac . c_erfc . realToFrac

instance Erf Float where
    erf = realToFrac . c_erff . realToFrac
    erfc = realToFrac . c_erfcf . realToFrac
