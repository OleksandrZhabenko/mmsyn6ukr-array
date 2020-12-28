{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Melodics.Ukrainian.Arr
-- Copyright   :  (c) OleksandrZhabenko 2019-2020
-- License     :  MIT
-- Maintainer  :  olexandr543@yahoo.com
--
-- Functions provide functionality of a musical instrument synthesizer or for Ukrainian speech synthesis
-- especially for poets, translators and writers.
--

module Melodics.Ukrainian.Arr (
  appendULFile
  , convertToProperUkrainian
  , isUkrainian
) where

import qualified Data.ByteString.Char8 as B
import System.IO
import CaseBi.Arr
import qualified Data.Foldable as F
import GHC.Arr
import qualified Melodics.ByteString.Ukrainian.Arr as MU (convertToProperUkrainianS,isUkrainianL)
import Paths_mmsyn6ukr_array

{-|
The first version has been initially inspired by: https://mail.haskell.org/pipermail/beginners/2011-October/008649.html
-}

-- | The function that actually produces a .raw file. The mapping table is given in the @Map.txt@ file.
appendULFile ::  [String] -> Handle -> IO ()
appendULFile xss hdl | not (null xss) =
  do
    dataFileList <- mapM getDataFileName
      ["-.ul", "0.ul", "1.ul", "A.ul", "B.ul", "C.ul", "D.ul", "E.ul", "F.ul", "G.ul", "H.ul",
        "I.ul", "J.ul", "K.ul", "L.ul", "M.ul", "N.ul", "O.ul", "P.ul", "Q.ul", "R.ul",
          "S.ul", "T.ul", "U.ul", "V.ul", "W.ul", "X.ul", "Y.ul", "Z.ul", "a.ul", "b.ul", "c.ul",
            "d.ul", "e.ul", "f.ul"]
    dataArray0 <- mapM B.readFile $! dataFileList
    let !dataArray = listArray (0,34) dataArray0
    mapM_ (\u ->
      if F.all (\z -> B.length z > 0) dataArray
        then let rs =  tail . dropWhile (/= ' ') . takeWhile (/= '}') . show $ hdl in do
          hClose hdl
          closedHdl <- hIsClosed hdl
          if closedHdl
            then do
                   B.appendFile rs $ unsafeAt dataArray . getBFstLSorted' 0 [("-", 0), ("0", 1), ("1", 2), ("а", 3), ("б", 4),
                     ("в", 5), ("г", 6), ("д", 7), ("дж", 8), ("дз", 9), ("е", 10), ("ж", 11), ("з", 12), ("и", 13),
                        ("й", 14), ("к", 15), ("л", 16), ("м", 17), ("н", 18), ("о", 19), ("п", 20), ("р", 21),
                          ("с", 22), ("сь", 23), ("т", 24), ("у", 25), ("ф", 26), ("х", 27), ("ц", 28), ("ць", 29),
                            ("ч", 30), ("ш", 31), ("ь", 32), ("і", 33), ("ґ", 34)] $ u
            else error "File is not closed!"
        else error "Data sound file is not read!") xss
    hClose hdl
                       | otherwise = return ()

-- | The function that converts a written Ukrainian text into the sounding in the program phonetical respesentation.
-- It is not exact phonetically but you can make for yourself a general impression of the Ukrainian sounding.
convertToProperUkrainian :: String -> [String]
convertToProperUkrainian xs = new2OldRepresentation . MU.convertToProperUkrainianS $ xs
  where new2OldRepresentation :: String -> [String]
        new2OldRepresentation ys = map f $ ys
          where f = getBFstLSorted' "" (zip "-01ABCDEFabcdefghijklmnopqrstuvwxyz" ["-","0","1","дз","ж","й","сь",
                  "ч","ш","а","б","ц","д","е","ф","ґ","г","і","дж","к","л","м","н","о","п","ь","р","с","т","у","в",
                    "ць","х","и","з"])

isUkrainian :: Char -> Bool
isUkrainian = MU.isUkrainianL
