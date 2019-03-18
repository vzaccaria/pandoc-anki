{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Backend.CrowdAnki.Render where

import Backend.CrowdAnki.Deck
import Backend.CrowdAnki.Note
import Backend.CrowdAnki.NoteModel
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate
import Data.Tree
import Internal.Parse
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

renderConceptToLatexNote :: String -> Concept -> Maybe Note
renderConceptToLatexNote hd q =
  let noteTitle = inlineToLatex . getTitle $ q
      noteContent = blockToLatex . getConceptContents $ q
      note =
        N
          [noteTitle, noteContent]
          0
          (defaultNoteWithHeader hd :: NoteModel)
          []
          ""
      opt = Map.lookup "noanki" (getHeadingMeta q)
  in case opt of
       Just _ -> Nothing
       Nothing -> Just note

internalDeckToDeck :: InternalDeck -> String -> Deck
internalDeckToDeck idk headdata =
  let renderConceptTree curlev cardlev s =
        if curlev == (cardlev - 1)
          then let (Node concept trees) = s
                   subdeckname = getName concept
                   notes =
                     mapMaybe
                       ((renderConceptToLatexNote headdata) . getNodeConcept)
                       trees
               in (def :: Deck) {d_notes = notes, d_name = subdeckname}
          else let (Node concept trees) = s
                   name =
                     if (curlev == 0)
                       then getDeckName idk
                       else getName concept
                   decks = map (renderConceptTree (curlev + 1) cardlev) trees
               in (def :: Deck) {d_children = decks, d_name = name}
  in renderConceptTree 0 (getCardLevel idk) $ getConceptTree idk

renderAsCrowdAnki :: InternalDeck -> String -> IO String
renderAsCrowdAnki d headdata = d_dumpStringIO $ internalDeckToDeck d headdata
