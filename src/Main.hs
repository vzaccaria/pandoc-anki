{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Backend.CrowdAnki.Render   as CA
import           Data.Aeson
import           Data.Aeson.Encode
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Internal.Parse
import qualified Internal.Parse             as IP
import           System.Console.Docopt.NoTH
import           System.Environment         (getArgs)
import           UsageCLI                   (progUsage)

split :: String -> String -> [String]
split sep str = map T.unpack $ T.splitOn (T.pack sep) (T.pack str)

readFileData :: Maybe String -> IO String
readFileData (Just filename) = readFile filename
readFileData Nothing         = return ""

dispatchOptions :: Docopt -> Arguments -> IO ()
dispatchOptions usage' opts = do
  let getBoolOpt x = isPresent opts (longOption x)
  let getStringOpt x = getArg opts (longOption x)
  let wantsInternal = getBoolOpt "internal"
  let wantsHtml = getBoolOpt "html"
  let wantsLatex = getBoolOpt "latex"
  let wantsJson = getBoolOpt "json"
  let wantsFlattened = getBoolOpt "flatten"
  filename <- getArgOrExitWith usage' opts (argument "FILE")
  filedata <- readFile filename
  headerData <- readFileData (getArg opts (longOption "header"))
  psed <- return $ IP.parseOrg filedata
  let fted =
        if wantsFlattened
          then flattenDeckLev psed 1
          else psed
  let ro = RO wantsHtml wantsLatex headerData
  ca <- CA.renderAsCrowdAnki fted ro
  nt <- renderAsInternal fted
  putStrLn $
    case (wantsJson, wantsInternal) of
      (True, False) -> ca
      (False, True) -> nt
      (True, True) -> BL.unpack $ encode $ toJSON fted
      _ -> error "Sorry, either internal, json or both must be specified"

main :: IO ()
main = do
  opts <- parseArgsOrExit progUsage =<< getArgs
  dispatchOptions progUsage opts
