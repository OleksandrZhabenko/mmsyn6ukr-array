{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Melodics.Executable.Arr
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- A program and a library that can be used as a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers.
--

module Melodics.Executable.Arr (
  circle
  , workWithInput
  , rawToSoundFile
  , printInfoF
  , printEnding
  , recFileName
)
where

import Data.Char (isSpace, isControl)
import Data.Maybe (isJust,fromJust)
import System.IO
import System.Process (callProcess)
import System.Directory (removeFile)
import Control.Exception (onException)
import EndOfExe (showE)
import Melodics.Ukrainian.Arr (appendULFile, convertToProperUkrainian)
import UkrainianLControl.Arr

-- | Is used to repeat the cycle of creation of the sound files in the current directory for the @mmsyn6ukr@  executable.
circle :: String -> Bool -> FilePath -> IO ()
circle zs onepass file
 | onepass = onException (workWithInput zs True file 1) (printEnding zs)
 | otherwise = onException (mapM_ (workWithInput zs False file) [1..]) (printEnding zs)

printEnding :: String -> IO ()
printEnding xs = do
  putStr "Notice, there was (may be) CmdLineArgument exception. To avoid it, please, specify the command line argument (if needed) in the form \"ABC\""
  putStr " where A is either a letter \'f\', \'o\', \'w\' or a digit and B and C are both digits! The exception (may be) arose from "
  putStrLn $ "the command line arguments " ++ show xs ++ ". Please, check also whether the SoX was installed with the support for needed codec"
{-# INLINE printEnding #-}

-- | Interactively creates sound files in the current directory for the Ukrainian text input. Is used internally in the 'circle'
workWithInput :: String -> Bool -> FilePath -> Int -> IO ()
workWithInput zs onepass file _ = do
 [nameSF,ys] <- if null file then nameAndControl zs onepass file [1,2] else nameAndControl zs onepass file [1,3]
 withBinaryFile (nameSF ++ ".ul") AppendMode (appendULFile (convertToProperUkrainian (if onepass then unwords . words $ ys else ys)))
 putStrLn "The .ul file was created by the program. If there is SoX installed then it will run further. "
 let ts = showE "sox"
 if isJust ts
   then rawToSoundFile zs nameSF (fromJust ts)
   else printInfoF

-- | Is used to retriev the user-defined file name for the record.
recFileName :: IO String
recFileName = do
  putStrLn "Please, specify the name of the resulting sound file. Please, do NOT use '}' character and space or control characters!"
  nameOfSoundFile <- getLine
  let nameSF = filter (\x -> not (isSpace x) && not (isControl x) && x /= '}') nameOfSoundFile
  return nameSF

getCtrl :: String -> Bool -> IO String
getCtrl zs onepass = do
  xs <- if onepass then getLine else getContents
  let ys = take (nSymbols . fst . genControl $ zs) xs
  return ys

recFNAndCtrl :: String -> Bool -> FilePath -> Int -> IO String
recFNAndCtrl zs onepass file n
  | n == 1 = recFileName
  | n == 3 = readFile file
  | otherwise = getCtrl zs onepass

nameAndControl :: String -> Bool -> FilePath -> [Int] -> IO [String]
nameAndControl zs onepass file = mapM (recFNAndCtrl zs onepass file)

-- | Converts RAW sound to the sound file of the needed format in the current directory accordingly to the 'genControl' for the first 'String' argument.
-- Is used internally in the 'workWithInput'.
rawToSoundFile :: String -> String -> FilePath -> IO ()
rawToSoundFile zs nameSF executablePath
  | null zs = do
     callProcess executablePath ["-r22050","-c1", nameSF ++ ".ul", "-r22050",nameSF ++ ".wav"]
     removeFile $ nameSF ++ ".ul"
  | otherwise = do
     let ws = snd . genControl $ zs
     callProcess executablePath (if snd ws /= ".wav" then ["-r22050","-c1", nameSF ++ ".ul", "-r22050",fst ws, nameSF ++ snd ws] else ["-r22050","-c1", nameSF ++ ".ul", fst ws, nameSF ++ snd ws])
     removeFile $ nameSF ++ ".ul"

-- | Prints informational message about ending of the possible for the given data program operation on sound files. Is used internally in the 'workWithInput'.
-- Is used internally in the 'workWithInput'.
printInfoF :: IO ()
printInfoF = do
  putStr "You have a resulting file in a raw PCM format with bitrate 22050 Hz and 1 channel (mono) in the .ul format. "
  putStr "You can further process it by yourself manually, otherwise, please, install SoX executable in the directory mentioned in the variable PATH and then run: "
  putStrLn "\"Path_to_the_SoX_executable\" -r22050 -c1 name_of_the_file_in_ul_format_with_new._prefix name_of_the_file_in_wav_format_with_new._prefix in the terminal."
