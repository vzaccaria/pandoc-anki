{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Deck.CrowdAnki.Render as CA
import qualified Deck.CSV.Render as CSV
import Deck.Parse
import Deck.RenderCommon
import System.Console.Docopt.NoTH
import System.Environment (getArgs)
import UsageCLI (progUsage)

split :: String -> String -> [String]
split sep str = map T.unpack $ T.splitOn (T.pack sep) (T.pack str)

dispatchOptions :: Docopt -> IO ()
dispatchOptions usage' =
    let wantsInternal opts = isPresent opts (longOption "internal")
        wantsJson opts = isPresent opts (longOption "json")
        wantsFlattened = (flip isPresent) (longOption "flatten")
        prog = do
            opts <- parseArgsOrExit usage' =<< getArgs
            file <- getArgOrExitWith usage' opts (argument "FILE")
            f <- readFile file
            psed <- return $ parseOrg f
            fted <-
                return $
                if wantsFlattened opts
                    then flattenDeck psed
                    else psed
            ca <- CA.renderAsCrowdAnki $ fted
            nt <- renderAsInternal $ fted
            cs <- CSV.renderAsCSV $ fted
            putStrLn $
                if wantsJson opts
                    then ca
                    else if wantsInternal opts
                             then nt
                             else cs
    in prog

main :: IO ()
main = dispatchOptions progUsage