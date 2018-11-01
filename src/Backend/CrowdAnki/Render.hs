{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.CrowdAnki.Render where

import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Backend.CrowdAnki.Deck
import Backend.CrowdAnki.Note
import Backend.CrowdAnki.NoteModel
import Internal.Parse
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate
import Data.Tree
import Text.Pandoc
import Text.Pandoc.Walk (walk)
import Utils

inlineToLatex :: [Inline] -> String
inlineToLatex i = blockToLatex $ [Plain i]

blockToLatex :: [Block] -> String
blockToLatex b =
    let ltx = writeLaTeX def (Pandoc (Meta Map.empty) b)
    in "[latex]" ++ ltx ++ "[/latex]"

getNodeConcept (Node q _) = q

renderConceptToLatexNote :: Concept -> Maybe Note
renderConceptToLatexNote q =
    let noteTitle = inlineToLatex . getTitle $ q
        noteContent = blockToLatex . getConceptContents $ q
        note = N [noteTitle, noteContent] 0 (def :: NoteModel) [] ""
        opt = Map.lookup "noanki" (getHeadingMeta q)
    in case opt of
           Just _ -> Nothing
           Nothing -> Just note

internalDeckToDeck :: InternalDeck -> Deck
internalDeckToDeck idk =
    let renderConceptTree curlev cardlev s =
            if curlev == (cardlev - 1)
                then let (Node concept trees) = s
                         subdeckname = getName concept
                         notes =
                             mapMaybe
                                 (renderConceptToLatexNote . getNodeConcept)
                                 trees
                     in (def :: Deck)
                        { d_notes = notes
                        , d_name = subdeckname
                        }
                else let (Node concept trees) = s
                         name =
                             if (curlev == 0)
                                 then getDeckName idk
                                 else getName concept
                         decks =
                             map (renderConceptTree (curlev + 1) cardlev) trees
                     in (def :: Deck)
                        { d_children = decks
                        , d_name = name
                        }
    in renderConceptTree 0 (getCardLevel idk) $ getConceptTree idk

renderAsCrowdAnki :: InternalDeck -> IO String
renderAsCrowdAnki d = d_dumpStringIO $ internalDeckToDeck d