{-# LANGUAGE TemplateHaskell #-}

module DramPaste.Words where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Paths_drampaste
import           System.Random

wordList :: V.Vector T.Text
wordList = $(do
    s <- runIO $ getDataFileName "wordlist/words.txt" >>= T.readFile
    [| V.fromList $(lift (T.lines s)) |])

randomMnemonic :: StdGen -> T.Text
randomMnemonic g =
    T.intercalate "-" $
        (wordList V.!) <$> take 3 (randomRs (0, V.length wordList - 1) g)
