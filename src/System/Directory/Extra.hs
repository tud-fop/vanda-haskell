-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory.Extra
-- Copyright   :  (c) Technische Universität Dresden 2016
-- License     :  BSD-style
--
-- Maintainer  :  Toni.Dietze@tu-dresden.de
-- Stability   :  unknown
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Directory.Extra where


import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents )
import System.FilePath ((</>))

import Control.Monad.Extra (ifM)


-- | For every given 'FilePath', recursively list all files that are not
-- directories. If a given 'FilePath' is not a directory, it is returned as
-- is. Non-existing files do not cause errors.
getDirectoryContentsRecursive :: [FilePath] -> IO [FilePath]
getDirectoryContentsRecursive paths
  = fmap concat
  $ forM paths $ \ path ->
      ifM (doesDirectoryExist path)
        ( getDirectoryContents path
          >>= getDirectoryContentsRecursive
            . map (path </>)
            . filter (`notElem` [".", ".."])  -- TODO: this seems like a hack
        )
        (return [path])
