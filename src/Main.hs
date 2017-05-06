{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import           Deck.RenderCSV
import           Deck.RenderJSON
import           System.Console.Docopt.NoTH
import           System.Environment         (getArgs)
import           UsageCLI                   (progUsage)

split :: String -> String -> [String]
split sep str = map T.unpack $ T.splitOn (T.pack sep) (T.pack str)

dispatchOptions :: Docopt -> IO ()
dispatchOptions usage' =
    let wantsInternal opts = isPresent opts (longOption "internal")
        wantsJson opts = isPresent opts (longOption "json")
        prog = do
            opts <- parseArgsOrExit usage' =<< getArgs
            file <- getArgOrExitWith usage' opts (argument "FILE")
            f <- readFile file
            putStrLn $
                if wantsJson opts
                    then renderFileJSON f
                    else if wantsInternal opts
                             then renderInternal f
                             else renderFileCSV f
    in prog

main :: IO ()
main = dispatchOptions progUsage
