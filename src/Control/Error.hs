-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Error
-- Description :  generate useful error messages
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  Redistribution and use in source and binary forms, with
--                or without modification, is ONLY permitted for teaching
--                purposes at Technische Universität Dresden AND IN
--                COORDINATION with the Chair of Foundations of Programming.
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- Helper functions to generate useful error messages.
--
-- The idea is to use qualified imports for this module and define partially
-- applied versions of the functions of this module, e.g.:
--
-- @
-- module Data.Example where
--
-- import qualified "Control.Error"
--
-- errorHere :: String -> String -> String
-- errorHere = "Control.Error".'errorHere' \"Data.Example\"
-- @
-----------------------------------------------------------------------------


module Control.Error
( errorHere
, messageHere
) where


-- | Call 'error' with a pretty error message, c.f. 'messageHere'.
errorHere
  :: String  -- ^ module name
  -> String  -- ^ function name
  -> String  -- ^ message
  -> a
errorHere m f = error . messageHere m f


-- | Create pretty error messages, even if an input is 'null'.
--
-- For example:
-- @'messageHere' \"Data.Example\" \"fun\" \"empty list\"
-- = \"Data.Example.fun: empty list\"@
messageHere
  :: String  -- ^ module name
  -> String  -- ^ function name
  -> String  -- ^ message
  -> String
messageHere m f x
  = m
  ++ (if null m || null f then "" else ".")
  ++ f
  ++ (if null m && null f || null x then "" else ": ")
  ++ x
