-- Copyright (C) 2020 Juri Dispan
--
-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program; if not, write to the Free Software Foundation, Inc., 59 Temple
-- Place, Suite 330, Boston, MA 02111-1307 USA

{-# LANGUAGE RecordWildCards #-}

module Tldr.App.Handler
  ( handleTldrOpts
  )
where

import           Data.Char                      ( toLower )
import           Data.List                      ( intercalate
                                                , delete
                                                , isPrefixOf
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Time
import           Data.Maybe                     ( isJust )
import           System.Directory
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.FilePath                ( (<.>)
                                                , (</>)
                                                )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )
import           Network.HTTP.Simple
import           Codec.Archive.Zip
import           Control.Monad

import           Tldr
import           Tldr.Types

tldrDirName :: String
tldrDirName = "tldr"

pagesUrl :: String
pagesUrl = "https://tldr.sh/assets/tldr.zip"

platformDirs :: [String]
platformDirs = ["linux", "osx", "windows", "sunos"]

showHelp :: Int -> IO ()
showHelp exitCode = do
  let
    helpMsg = unlines
      [ "Usage:"
      , "\ttldr [options] COMMAND"
      , "\ttldr [options]"
      , "Options:"
      , "-h --help                  Show help"
      , "-u --update                Update local cache"
      , "-L --language <lang>       Use <lang> instead of english"
      , "-p --platform <platform>   Use <platform> instead of the native platform"
      ]
  putStr helpMsg
  if exitCode == 0 then exitSuccess else exitFailure

getLocaleSuffix :: Maybe String -> Maybe String
getLocaleSuffix lang = case map toLower <$> lang of
  Nothing  -> Nothing
  Just loc -> if "en" `isPrefixOf` loc then Nothing else Just loc

handleTldrOpts :: Command -> IO ()
handleTldrOpts command = case command of
  UpdateIndex             -> updateTldrPages
  ShowHelp exitCode       -> showHelp exitCode
  ViewPage voptions pages -> do
    shouldPerformUpdate <- updateNecessary command
    when shouldPerformUpdate updateTldrPages
    let npage        = intercalate "-" pages
    let localeSuffix = getLocaleSuffix $ languageOption voptions
    fname <- getPagePath localeSuffix npage (getCheckDirs voptions)
    case fname of
      Just path -> renderPage path
      Nothing   -> if isJust localeSuffix
        then handleTldrOpts
          $ ViewPage voptions { languageOption = Nothing } pages
        else do
          hPutStrLn stderr ("No tldr entry for " <> unwords pages)
          exitFailure

updateNecessary :: Command -> IO Bool
updateNecessary (ViewPage ViewOptions {..} _) = do
  dataDir       <- getXdgDirectory XdgData tldrDirName
  dataDirExists <- doesDirectoryExist dataDir
  if not dataDirExists
    then return True
    else case autoUpdateInterval of
      Nothing             -> return False
      Just updateInterval -> do
        lastCachedTime <- getModificationTime dataDir
        currentTime    <- getCurrentTime
        let diffExceedsLimit =
              currentTime
                `diffUTCTime` lastCachedTime
                >             fromIntegral updateInterval
                *             nominalDay
        return diffExceedsLimit
updateNecessary _ = return False

updateTldrPages :: IO ()
updateTldrPages = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  removePathForcibly dataDir
  createDirectory dataDir
  putStrLn $ "Downloading tldr pages to " ++ dataDir
  response <- httpLBS $ parseRequest_ pagesUrl
  let zipArchive = toArchive $ getResponseBody response
  extractFilesFromArchive [OptDestination dataDir] zipArchive

getPagePath :: Maybe String -> String -> [String] -> IO (Maybe FilePath)
getPagePath localeSuffix page pDirs = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let currentLocaleDir = case localeSuffix of
        Nothing -> "pages"
        Just xs -> "pages." <> xs
      pageDir = dataDir </> currentLocaleDir
      paths   = map (\x -> pageDir </> x </> page <.> "md") pDirs
  getFirstExisting paths

getFirstExisting :: [FilePath] -> IO (Maybe FilePath)
getFirstExisting []       = return Nothing
getFirstExisting (x : xs) = do
  exists <- doesFileExist x
  if exists then return $ Just x else getFirstExisting xs

getCheckDirs :: ViewOptions -> [String]
getCheckDirs voptions = case platformOption voptions of
  Nothing       -> "common" : platformDirs
  Just platform -> "common" : platform : delete platform platformDirs
