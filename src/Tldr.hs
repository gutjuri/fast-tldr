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

{-# LANGUAGE OverloadedStrings #-}

module Tldr
  ( renderPage
  )
where

import           Data.Monoid                    ( (<>) )
import           Data.Text               hiding ( cons )
import           System.Console.ANSI
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

renderHeading :: Text -> IO ()
renderHeading ln = do
  setSGR [SetColor Foreground Dull White]
  TIO.putStrLn $ "== " <> T.drop 2 ln <> " ==\n"

renderDescription :: Text -> IO ()
renderDescription ln = do
  setSGR [SetColor Foreground Dull White]
  TIO.putStrLn $ T.drop 2 ln <> "\n"

renderCommandDescription :: Text -> IO ()
renderCommandDescription ln = do
  setSGR [SetColor Foreground Dull Green]
  TIO.putStrLn $ T.drop 2 ln

renderCommand :: Text -> IO ()
renderCommand ln = do
  setSGR [SetColor Foreground Dull Blue]
  TIO.putStrLn $ "\t" <> T.dropEnd 1 (T.drop 1 ln) <> "\n"

renderLine :: Text -> IO ()
renderLine ln | "#" `T.isPrefixOf` ln = renderHeading ln
              | ">" `T.isPrefixOf` ln = renderDescription ln
              | "-" `T.isPrefixOf` ln = renderCommandDescription ln
              | "`" `T.isPrefixOf` ln = renderCommand ln
              | "" == ln              = return ()
              | otherwise             = error "Malformed TLDR-Page"

renderPage :: FilePath -> IO ()
renderPage fname = do
  text <- TIO.readFile fname
  mapM_ renderLine $ T.lines text
