{-# LANGUAGE QuasiQuotes #-}

module UsageCLI (progUsage) where

import           System.Console.Docopt
import           System.Environment    (getArgs)

progUsage :: Docopt
progUsage =
    [docopt|
pandoc-anki

Usage:
    pandoc-anki FILE [-j | -i]
    pandoc-anki --help | -h
    pandoc-anki --version

Options:
    -i, --internal         Dump internal tree representation
    -j, --json             Output JSON suitable for CrowdAnki
    -h, --help             Show help
    --version              Show version.

Arguments
    FILE                   Org file containing the mindmap
|]
