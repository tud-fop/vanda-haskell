-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.CmdArgs.Explicit.Misc
-- Description :  convenience functions for "System.Console.CmdArgs.Explicit"
-- Copyright   :  (c) Technische Universität Dresden 2014
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
--
-- This module provides some convenience functions building on top of
-- "System.Console.CmdArgs.Explicit".
-----------------------------------------------------------------------------

module System.Console.CmdArgs.Explicit.Misc
( -- * Parsing
  readUpdate
, readEither
, -- * Help
  defaultHelp
, populateHelpFlag
, populateHelpMode
) where


import Data.Typeable (Typeable, typeOf)
import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Text


-- | Define 'Update' functions for instances of 'Read' (and 'Typeable').
-- Produces readable error messages, if parsing fails.
readUpdate :: (Read r, Typeable r) => (r -> a -> a) -> Update a
readUpdate update a x = case readEither a of
  Right r  -> Right (update r x)
  Left typ -> Left  ("expected value of type " ++ typ)


-- | Parse a 'String' to some type like 'read' and return the result wrapped
-- in 'Right', if the parse succeeded, otherwise return the type as 'String'
-- wrapped in 'Left'.
readEither :: (Read r, Typeable r) => String -> Either String r
readEither cs = case reads cs of
  (x, "") : _ -> Right x
  typ         -> Left $ show $ typeOf $ fst $ head typ


-- | Render the help for a mode with default rendering options.
defaultHelp :: Mode a -> String
defaultHelp = showText defaultWrap . helpText [] HelpFormatDefault


-- | Add 'flagHelpFormat' to every (sub-)mode.
--
-- If any (sub-)mode has required arguments, you shall prefer
-- 'populateHelpMode'.
populateHelpFlag
  :: (String -> a)  -- ^ applied to the help texts of the (sub-)modes
  -> Mode a
  -> Mode a
populateHelpFlag ctor m = m'
  where
    m' = m
      { modeGroupModes = fmap (populateHelpFlag ctor) (modeGroupModes m)
      , modeGroupFlags = let g = modeGroupFlags m
                        in g{groupUnnamed = groupUnnamed g
                                          ++ [flagHelpFormat update]}
      }
    update helpFormat textFormat _
      = ctor
      $ showText textFormat
      $ helpText [] helpFormat m


-- | Adds a sub-mode help to every mode, extends the 'modeHelpSuffix', and
-- replaces the 'modeValue' at the root by @update helpText@.
-- The help modes take an optional argument like 'flagHelpFormat'.
populateHelpMode
  :: (String -> a)  -- ^ applied to the help texts of the (sub-)modes
  -> Mode a
  -> Mode a
populateHelpMode ctor m = populateHelpMode' ctor m'
  where
    m' = m
      { modeValue = ctor $ defaultHelp m'
      , modeHelpSuffix = modeHelpSuffix m
        ++ ["Every mode provides a sub-mode help, which displays only \
            \help concerning this mode. Every help mode has an optional \
            \argument which defines the help format."]
      }

populateHelpMode' :: (String -> a) -> Mode a -> Mode a
populateHelpMode' ctor m = m'
  where
    m' = m
      { modeGroupModes
          = let g = fmap (populateHelpMode' ctor) (modeGroupModes m)
            in g{groupUnnamed = groupUnnamed g ++ [helpMode]}
      }
    helpMode = (modeEmpty $ ctor $ defaultHelp m)
      { modeNames = ["help"]
      , modeHelp  = flagHelp helpFlag
      , modeArgs  = ([helpArg], Nothing)
      }
    helpArg = flagArg (flagValue helpFlag) (flagType helpFlag)
    helpFlag = flagHelpFormat update
    update helpFormat textFormat _
      = ctor
      $ showText textFormat
      $ helpText [] helpFormat m
