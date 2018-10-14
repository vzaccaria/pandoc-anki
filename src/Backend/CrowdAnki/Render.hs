{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Backend.CrowdAnki.Render where

import           Data.Aeson.Encode.Pretty 
import qualified Data.ByteString.Lazy.Char8 as BL
import           Backend.CrowdAnki.Deck
import           Backend.CrowdAnki.Note
import           Backend.CrowdAnki.NoteModel
import           Internal.Parse
import           Data.List
import           Data.List.Split
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.String.Interpolate
import           Data.Tree
import           Text.Pandoc
import           Text.Pandoc.Walk            (walk)
import           Utils

processLeafContentsAsLatex :: [Block] -> String
processLeafContentsAsLatex b = 
  let ltx = writeLaTeX def (Pandoc (Meta Map.empty) b) 
   in "[latex]" ++ ltx ++"[/latex]"

renderLeafAsLatex :: InternalDeckStructLeaf -> String
renderLeafAsLatex l = processLeafContentsAsLatex . contents $ l

renderStructIntoNote :: InternalDeckStruct -> Maybe Note   
renderStructIntoNote (Node q _) =
  let noteTitle = [i|[latex]#{getName q}[/latex]|]
      noteContent = renderLeafAsLatex q
      note = N [noteTitle, noteContent] 0 (def :: NoteModel) [] ""
      opt = Map.lookup "noanki" (getHeadingMeta q)
  in case opt of 
       Just _  -> Nothing
       Nothing -> Just note

renderStructure :: Integer -> Integer -> InternalDeck -> InternalDeckStruct -> Deck
renderStructure curlev cardlev idk s =
  if curlev == (cardlev - 1)
    then let (Node concept childs) = s
             name = getName concept
             notes = mapMaybe renderStructIntoNote childs
         in (def :: Deck) {d_notes = notes, d_name = name}
    else let (Node concept childs) = s
             name =
               if (curlev == 0)
                 then getTitle idk
                 else getName concept
             decks = map (renderStructure (curlev + 1) cardlev idk) childs
         in (def :: Deck) {d_children = decks, d_name = name}

internalDeckToDeck :: InternalDeck -> Deck
internalDeckToDeck d = renderStructure 0 (getCardLevel d) d $ getInternalDeckStruct d

renderAsCrowdAnki :: InternalDeck -> IO String
renderAsCrowdAnki d = d_dumpStringIO $ internalDeckToDeck d


