{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Deck.Render where

import           Data.Char               (toLower)
import           Data.List
import           Data.List.Split
import qualified Data.Map                as Map
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Tree
import           Debug.Trace
import           Deck.Parse
import           System.Directory
import           System.Process
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.Walk        (walk)
import           Utils


processLeafContents :: [Block] -> String
processLeafContents b = writeHtmlString def (Pandoc (Meta Map.empty) b)

processInline :: Inline -> Inline
processInline (Math _ x) = Str [i|[latex]$#{x}$[/latex]|]
processInline x = x

processBlock :: Block -> Block
processBlock = walk processInline

processPandoc :: Pandoc -> Pandoc
processPandoc = walk processBlock

unLines s = intercalate "" $ splitOn "\n" s


renderLeaf :: StructureLeaf -> String
renderLeaf l = unLines $ (processLeafContents . contents $ l)

renderCard :: String -> Structure -> String
renderCard ctx (Node q [Node a _]) =
  let
    q' = renderLeaf q
    a' = renderLeaf a
  in [i|#{q'};#{a'}\n|]
renderCard _ _ = error "Invalid syntax for card"

renderContext :: Structure -> String
renderContext (Node ctx cards) = concatMap (renderCard (getName ctx)) cards

_drawAsForest :: String -> IO ()
_drawAsForest f =
  let
    s :: Structure
    s = getStructure $ parseDeck $ processPandoc $ readDoc f
    t = fmap show s
    t' = drawTree t
  in putStrLn t'


renderFile :: String -> String
renderFile s = let
  (Node _ ctxs) = getStructure $ parseDeck $ processPandoc $ readDoc s
  s' = intercalate "\n" (map renderContext ctxs)
  in "Front;Back\n" ++ s'

