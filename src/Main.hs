{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad              (when)
import qualified Data.Map                   as M
import           Data.String
import qualified Data.Text                  as T
import           Debug.Trace
import           Deck.Parse
import           Deck.Render
import           Filesystem.Path.CurrentOS  (encodeString, filename,
                                             replaceExtension)
import           Prelude                    (Bool (..), IO (..), Maybe (..),
                                             map, not, print, putStrLn,
                                             readFile, return, ($), (<$>),
                                             (=<<), (>>=))
import           System.Console.Docopt.NoTH
import           System.Environment         (getArgs)
import           System.Exit
import           System.Process
import           UsageCLI                   (progUsage)
import           Utils

split :: String -> String -> [String]
split sep str =
  map T.unpack $
  T.splitOn (T.pack sep)
            (T.pack str)

dispatchOptions :: Docopt -> IO ()
dispatchOptions usage =
  let prog =
        do opts <- parseArgsOrExit usage =<< getArgs
           file <-
             getArgOrExitWith usage
                              opts
                              (argument "FILE")
           f <- readFile file
           putStrLn $ renderFile f
  in prog

main :: IO ()
main = dispatchOptions progUsage
