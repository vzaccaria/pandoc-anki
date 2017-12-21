{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Deck.CrowdAnki.Render where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map
import Data.String.Interpolate
import Data.Tree
import Deck.CrowdAnki.Deck
import Deck.CrowdAnki.Note
import Deck.CrowdAnki.NoteModel
import Deck.Parse
import Deck.RenderCommon
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Utils

renderCardJSON :: Structure -> Maybe Note
renderCardJSON (Node q _) =
    let q' = getName q
        a' = renderLeaf q
        note = N [q', a'] 0 (def :: NoteModel) [] ""
        opt = Map.lookup "noanki" (getHeadingMeta q)
    in case opt of
           Just _ -> Nothing
           Nothing -> Just note

renderStructure :: Integer -> Integer -> InternalDeck -> Structure -> Deck
renderStructure curlev cardlev idk s =
    if curlev == (cardlev - 1)
        then let (Node concept childs) = s
                 name = getName concept
                 notes = mapMaybe renderCardJSON childs
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

internalDeckToDeck :: InternalDeck -> Deck
internalDeckToDeck d = renderStructure 0 (getCardLevel d) d $ getStructure d

render :: String -> IO String
render s =
    d_dumpStringIO $ internalDeckToDeck $ parseDeck $ processPandoc $ readDoc s