{-# OPTIONS_GHC -fno-warn-incomplete-record-updates #-}

{-|
Module:      Vanda.Dyck.Main
Description: membership in /Dyck languages/ and /congruence multiple Dyck languages/
Copyright:   Ⓒ Tobias Denkinger, 2015
Maintainer:  Tobias.Denkinger@tu-dresden.de
Stability:   experimental

This program decides the membership of /Dyck languages/ and /congruence multiple Dyck languages/ [cf. <http://fsmnlp2015.phil.hhu.de/wp-content/uploads/2015/06/denkinger-chomsky-schuetzenberger.pdf Tobias Denkinger: A Chomsky-Schützenberger result for weighted multiple context-free languages, 2015>].
-}
module Vanda.Dyck.Main
( main
, mainArgs
, cmdArgs
, Args()
) where


import Vanda.Dyck.DyckLanguages (isDyck)
import Vanda.Dyck.MultipleDyckLanguages (isMultipleDyck)

import System.Console.CmdArgs.Explicit
import System.Console.CmdArgs.Explicit.Misc

data Args
  = Help String
  | MemberDyck
    { argLParens :: String
    , argRParens :: String
    , argWord :: String
    }
  | MemberMultipleDyck
    { argLParens :: String
    , argRParens :: String
    , argWord :: String
    }
  deriving Show

cmdArgs :: Mode Args
cmdArgs
  = modes "brackets" (Help $ defaultHelp cmdArgs) "algorithms for Dyck and multiple Dyck languages"
  [ (modeEmpty $ MemberDyck undefined undefined undefined)
    { modeNames = ["dyck", "Dyck"]
    , modeHelp = "checks whether WORD is member of the Dyck language given by LEFT_PARENTHESES and RIGHT_PARENTHESES."
    , modeArgs = ( [ flagArgLParens{argRequire = True}, flagArgRParens{argRequire = True}, flagArgWord{argRequire = True} ], Nothing )
    }
  , (modeEmpty $ MemberMultipleDyck undefined undefined undefined)
    { modeNames = ["mdyck", "mDyck", "m-dyck", "m-Dyck", "multipleDyck"]
    , modeHelp = "checks whether WORD is member of the multiple Dyck language given by LEFT_PARENTHESES and RIGHT_PARENTHESES. Linked parentheses are grouped and the groups are separated by any character from the string \"" ++ separators ++ "\"."
    , modeArgs = ( [ flagArgMLParens{argRequire = True}, flagArgMRParens{argRequire = True}, flagArgMWord{argRequire = True} ], Nothing )
    }
  ]
  where
    flagArgLParens
      = flagArg (\ a x -> Right x{argLParens = a}) "LEFT_PARENTHESES"
    flagArgRParens
      = flagArg (\ a x -> Right x{argRParens = a}) "RIGHT_PARENTHESES"
    flagArgWord
      = flagArg (\ a x -> Right x{argWord = a}) "WORD"
    flagArgMLParens
      = flagArg (\ a x -> Right x{argLParens = a}) "LEFT_PARENTHESES"
    flagArgMRParens
      = flagArg (\ a x -> Right x{argRParens = a}) "RIGHT_PARENTHESES"
    flagArgMWord
      = flagArg (\ a x -> Right x{argWord = a}) "WORD"


main :: IO ()
main = processArgs (populateHelpMode Help cmdArgs) >>= mainArgs

mainArgs :: Args -> IO ()

mainArgs (Help cs) = putStr cs

mainArgs (MemberDyck k1 k2 ws)
  = do putStrLn . show $ isDyck k1 k2 ws

mainArgs (MemberMultipleDyck k1 k2 ws)
  = do putStrLn . show $ isMultipleDyck separators k1 k2 ws

separators :: [Char]
separators = " ,;.:|"