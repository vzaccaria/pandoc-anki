{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Backend.CrowdAnki.Render as CA
import qualified Internal.Parse as IP
import Data.Aeson
import Data.Aeson.Encode
import Internal.Parse
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
            psed <- return $ IP.parseOrg f
            fted <-
                return $
                if wantsFlattened opts
                    then flattenDeck psed
                    else psed
            ca <- CA.renderAsCrowdAnki $ fted
            nt <- renderAsInternal $ fted
            putStrLn $
                case (wantsJson opts, wantsInternal opts) of
                    (True,False) -> ca
                    (False,True) -> nt
                    (True,True) -> BL.unpack $ encode $ toJSON fted
                    _ -> error "Sorry, either internal, json or both must be specified"
    in prog

main :: IO ()
main = dispatchOptions progUsage
