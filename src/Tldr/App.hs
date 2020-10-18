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

module Tldr.App
  ( appMain
  )
where

import           Data.List                      ( isPrefixOf )
import           System.Environment             ( getArgs )
import           Tldr.App.Handler
import           Tldr.Types
import           Control.Monad
import           Text.Read

setIfView :: ViewOptions -> Command -> Command
setIfView (ViewOptions po lo aui) (ViewPage (ViewOptions po' lo' aui') ps) =
  ViewPage (ViewOptions (po `mplus` po') (lo `mplus` lo') (aui `mplus` aui')) ps
setIfView _ x = x

parseNoFlags :: [String] -> Command
parseNoFlags = ViewPage (ViewOptions Nothing Nothing Nothing)

addCommand :: String -> Command -> Command
addCommand p (ViewPage vo ps) = ViewPage vo (p : ps)
addCommand _ x                = x

parseOpts :: [String] -> Command
parseOpts (x : _) | x `elem` ["-u", "--update"] = UpdateIndex
parseOpts (x : _) | x `elem` ["-h", "--help"]   = ShowHelp 0
parseOpts (x : _) | x `elem` ["-v", "--version"]   = ShowVersion
parseOpts (x : y : xs) | x `elem` ["-L", "--language"] =
  setIfView (ViewOptions Nothing (Just y) Nothing) $ parseOpts xs
parseOpts (x : y : xs) | x `elem` ["-p", "--platform"] =
  setIfView (ViewOptions (Just y) Nothing Nothing) $ parseOpts xs
parseOpts (x : y : xs) | x `elem` ["-a", "--auto-update-interval"] =
  case readMaybe y of
    Nothing -> ShowHelp 1
    updateInterval@(Just _) ->
      setIfView (ViewOptions Nothing Nothing updateInterval) $ parseOpts xs
parseOpts ("--" : xs) = parseNoFlags xs
parseOpts (x : xs) | not ("-" `isPrefixOf` x) = addCommand x $ parseOpts xs
                   | otherwise                = ShowHelp 1
parseOpts [] = ViewPage (ViewOptions Nothing Nothing Nothing) []

appMain :: IO ()
appMain = do
  args <- getArgs
  let opts = parseOpts args
  handleTldrOpts opts
