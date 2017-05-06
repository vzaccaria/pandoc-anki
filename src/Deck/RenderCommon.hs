{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.RenderCommon where

import           Data.List
import           Data.List.Split
import qualified Data.Map                as Map
import           Data.Maybe
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

showDeck :: InternalDeck -> String
showDeck d = drawTree $ fmap showSL (getStructure d)

showSL :: StructureLeaf -> String
showSL s = getName s ++ ": " ++ renderLeaf s

_drawAsForest :: String -> String
_drawAsForest f =
    let d = parseDeck $ processPandoc $ readDoc f
        author = getAuthor d
        title = getTitle d
        content = showDeck d
        l = show (getCardLevel d)
    in [i|#{title} - #{author} (encoding cards from level #{l} downwards (Root = 0))
#{content}
       |]
