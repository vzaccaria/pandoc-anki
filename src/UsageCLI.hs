{-# LANGUAGE QuasiQuotes #-}
module UsageCLI (progUsage) where

import System.Environment (getArgs)
import System.Console.Docopt

progUsage :: Docopt
progUsage = [docopt|
pandoc-anki

Usage:
    pandoc-anki FILE
    pandoc-anki --help | -h
    pandoc-anki --version

Options:
    -h, --help             Show help
    --version              Show version.

Arguments
    FILE                   Org file containing the mindmap
|]
