{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.RenderJSON where

import           Data.List
import           Data.List.Split
import qualified Data.Map                as Map
import           Data.String.Interpolate
import           Data.Tree
import           Deck.Crowd.Deck
import           Deck.Crowd.Note
import           Deck.Crowd.NoteModel
import           Deck.Parse
import           Deck.RenderCommon
import           Text.Pandoc
import           Text.Pandoc.Walk        (walk)
import           Utils

renderCardJSON :: Structure -> Note
renderCardJSON (Node q [Node a _]) =
    let q' = renderLeaf q
        a' = renderLeaf a
    in N [q', a'] 0 (def :: NoteModel) [] ""

renderStructure :: Integer -> Integer -> InternalDeck -> Structure -> Deck
renderStructure curlev cardlev idk s =
    if curlev == (cardlev - 1)
        then let (Node concept childs) = s
                 name = getName concept
                 notes = map renderCardJSON childs
             in (def :: Deck)
                { d_notes = notes
                , d_name = name
                }
        else let (Node concept childs) = s
                 name =
                     if (curlev == 0)
                         then getTitle idk
                         else getName concept
                 decks = map (renderStructure (curlev + 1) cardlev idk) childs
             in (def :: Deck)
                { d_children = decks
                , d_name = name
                }

renderInternalDeck :: InternalDeck -> Deck
renderInternalDeck d = renderStructure 0 (getCardLevel d) d $ getStructure d

renderFileJSON :: String -> String
renderFileJSON s =
    d_dumpString $ renderInternalDeck $ parseDeck $ processPandoc $ readDoc s
