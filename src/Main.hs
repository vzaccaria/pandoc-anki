{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import           Deck.RenderCSV
import           System.Console.Docopt.NoTH
import           System.Environment         (getArgs)
import           UsageCLI                   (progUsage)

split :: String -> String -> [String]
split sep str = map T.unpack $ T.splitOn (T.pack sep) (T.pack str)

dispatchOptions :: Docopt -> IO ()
dispatchOptions usage' =
    let prog = do
            opts <- parseArgsOrExit usage' =<< getArgs
            file <- getArgOrExitWith usage' opts (argument "FILE")
            f <- readFile file
            putStrLn $ renderFileCSV f
    in prog

main :: IO ()
main = dispatchOptions progUsage
