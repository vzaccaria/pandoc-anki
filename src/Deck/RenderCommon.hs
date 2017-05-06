{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.RenderCommon where

import           Data.List
import           Data.List.Split
import qualified Data.Map                as Map
import           Data.String.Interpolate
import           Data.Tree
import           Deck.Parse
import           Text.Pandoc
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

unLines :: String -> String
unLines s = intercalate "" $ splitOn "\n" s

renderLeaf :: StructureLeaf -> String
renderLeaf l = unLines (processLeafContents . contents $ l)

_drawAsForest :: String -> IO ()
_drawAsForest f =
    let s :: Structure
        s = getStructure $ parseDeck $ processPandoc $ readDoc f
        t = fmap show s
        t' = drawTree t
    in putStrLn t'
