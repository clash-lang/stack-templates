#!/usr/bin/env stack
{- stack script
 --resolver lts-24.34
 --install-ghc
 --package bytestring
 --package directory
 --package filepath
 --package utf8-string
 --profile
 --ghc-options -Wall
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}


-- base
import Control.Monad
import Data.List (sort)
import GHC.Stack

-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

-- directory
import System.Directory

-- filepath
import System.FilePath

-- | Like 'listDirectory', but recursive
listFilesRec ::
  HasCallStack =>
  FilePath ->
  IO [FilePath]
listFilesRec dir = do
  dirContents <- listDirectory dir
  fmap concat $ forM dirContents $ \p -> do
    isDir <- doesDirectoryExist (dir </> p)
    if isDir then do
      subfiles <- listFilesRec (dir </> p)
      pure (map (p </>) subfiles)
    else
      pure [p]

-- | Read project directory and gather files into a '.hsfiles' file
mkStackTemplate ::
  HasCallStack =>
  -- | Directory in projects/
  FilePath ->
  IO ByteString
mkStackTemplate projectDir = do
  contents <- sort <$> listFilesRec projectDir
  fmap mconcat $ forM contents $ \p -> do
    pContents <- BS.readFile (projectDir </> p)
    pure $
         "{-# START_FILE " <> BS.fromString p <> " #-}"
      <> "\n" <> pContents <> "\n"

-- | Read files in project/ and collect them into .hsfiles
main :: HasCallStack => IO ()
main = do
  let here = dropFileName __FILE__
  setCurrentDirectory here

  let projectsDir = here </> "projects"
  projects <- listDirectory projectsDir

  void $ forM projects $ \p -> do
    hsfiles <- mkStackTemplate (projectsDir </> p)
    putStrLn $ p <> ".hsfiles"
    BS.writeFile (here </> p <> ".hsfiles") hsfiles
