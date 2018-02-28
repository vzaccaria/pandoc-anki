{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Deck.CSV.Render where

import Data.List
import Data.String.Interpolate
import Data.Tree
import Deck.Parse
import Deck.RenderCommon
import Utils

renderCardCSV :: String -> Structure -> String
renderCardCSV _ (Node q [Node a _]) =
    let q' = renderLeaf q
        a' = renderLeaf a
    in [i|#{q'};#{a'}\n|]
renderCardCSV _ _ = error "Invalid syntax for card"

renderContextCSV :: Structure -> String
renderContextCSV (Node ctx cards) =
    concatMap (renderCardCSV (getName ctx)) cards

renderAsCSV :: InternalDeck -> IO String
renderAsCSV s =
    let (Node _ ctxs) = getStructure s
        s' = intercalate "\n" (map renderContextCSV ctxs)
    in return $ "Front;Back\n" ++ s'