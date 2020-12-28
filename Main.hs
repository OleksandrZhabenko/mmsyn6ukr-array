-- |
-- Module      :  Main
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers.
--

module Main where

import System.Environment (getArgs)
import Melodics.Executable.Arr

{-
-- Inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

-- | The function creates a raw .ul sound file with bitrate 22050 Hz 1 mono channel
-- and tries to automatically convert it to the .wav, .ogg, or .flac file with the same parameters specified by the first command line argument
-- (for more details see: 'genControl' function) using the system binary SoX (this is done for one circle of running, afterwards it is repeated
-- with the same command line arguments. To stop execution, please, interrupt the program e. g. with Ctrl + C on many Unix platforms).
-- So actually, it can create multiple sound files, all in the same format options specified by the first command line argument accordingly to
-- the 'genControl' function.
--
-- If SoX binaries are not installed properly, the program makes ending informational message for the user.
--
-- The first command line argument is the 'UkrainianLControl.Arr.genControl' specification.
--
-- The second command line argument (if specified) must be in the form \"+O\" and then if specified denotes that
-- the program cycle runs only once and exits after creating the single converted sound file. If specified, then
-- the program reads not the line of the input, but the contents and, therefore, it can use multiline contents (e. g., poetry).
--
-- The third command line argument is the filepath to the file with the Ukrainian text that instead of the user
-- input provided otherwise into the prompting line on the terminal is read to be converted.
--
-- The best comression ratio is with the .ogg files, but they lose some quality so be careful if you need it.
main :: IO ()
main = do
  args <- getArgs
  let zs = concat . take 1 $ args
      ts = concat . drop 1 . take 2 $ args
      file = concat . drop 2 . take 3 $ args
      onepass = if ts == "+O" then True else False
  circle zs onepass file
