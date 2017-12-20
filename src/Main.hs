{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import qualified Deck.CrowdAnki.Render      as CA
import qualified Deck.CSV.Render            as CSV
import qualified Deck.Internal.Render       as I
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
            renderedFile <- CA.render f
            putStrLn $
                if wantsJson opts
                    then renderedFile
                    else if wantsInternal opts
                             then I.render f
                             else CSV.render f
    in prog

main :: IO ()
main = dispatchOptions progUsage
