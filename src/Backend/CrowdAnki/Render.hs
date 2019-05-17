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

inlineToHtml :: [Inline] -> String
inlineToHtml i = blockToHtml $ [Plain i]

blockToHtml :: [Block] -> String
blockToHtml b =
    writeHtmlString def (Pandoc (Meta Map.empty) b)

inlineTransform :: RenderOptions -> ([Inline] -> String)
inlineTransform ro = if (wantsHtml ro) then inlineToHtml else inlineToLatex

blockTransform :: RenderOptions -> ([Block] -> String)
blockTransform ro = if (wantsHtml ro) then blockToHTML else blockToLatex

getNodeConcept (Node q _) = q

renderConceptToNote :: RenderOptions -> Concept -> Maybe Note
renderConceptToNote ro q =
  let noteTitle = (inlineTransform ro) . getTitle $ q
      noteContent = (blockTransform ro) . getConceptContents $ q
      note =
        N
          [noteTitle, noteContent]
          0
          (defaultNoteWithHeader $ getHeader ro :: NoteModel)
          []
          ""
      opt = Map.lookup "noanki" (getHeadingMeta q)
  in case opt of
       Just _ -> Nothing
       Nothing -> Just note

internalDeckToDeck :: InternalDeck -> RenderOptions -> Deck
internalDeckToDeck idk ro =
  let renderConceptTree curlev cardlev s =
        if curlev == (cardlev - 1)
          then let (Node concept trees) = s
                   subdeckname = getName concept
                   notes =
                     mapMaybe
                       ((renderConceptToNote ro) . getNodeConcept)
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

renderAsCrowdAnki :: InternalDeck -> RenderOptions -> IO String
renderAsCrowdAnki d ro = d_dumpStringIO $ internalDeckToDeck d ro
