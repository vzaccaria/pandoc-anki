{-# LANGUAGE QuasiQuotes #-}

module UsageCLI
  ( progUsage
  ) where

import System.Console.Docopt
import System.Environment (getArgs)

progUsage :: Docopt
progUsage =
  [docopt|
pandoc-anki

Usage:
    pandoc-anki FILE [-j] [-i] [-f] [-H] [-l] [-r <header>]
    pandoc-anki --help | -h
    pandoc-anki --version

Options:
    -i, --internal         Dump internal tree representation
    -f, --flatten          Remove all hierarchies
    -o, --flattenOne       Flatten to level 1
    -j, --json             Output JSON suitable for CrowdAnki
    -H, --html             Render text as HTML
    -l, --latex            Render text as LaTeX
    -r, --header <header>  Latex header to use for every card
    -h, --help             Show help
    --version              Show version.

Arguments
    FILE                   Org file containing the mindmap
|]
