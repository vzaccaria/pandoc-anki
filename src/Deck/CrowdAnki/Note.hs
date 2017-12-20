{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.CrowdAnki.Note where

import           Data.Aeson
import           Deck.CrowdAnki.NoteModel
import           GHC.Generics
import           Utils

data Note = N
    { n_fields     :: [String]
    , n_flags      :: Integer
    , n_note_model :: NoteModel
    , n_tags       :: [String]
    , n_guid       :: String
    } deriving (Show,Generic,Eq)

instance ToJSON Note where
    toJSON d =
        object
            [ "__type__" .= ("Note" :: String)
            , "data" .= ("" :: String)
            , "fields" .= n_fields d
            , "flags" .= n_flags d
            , "guid" .= n_guid d
            , "note_model_uuid" .= (nm_uuid $ n_note_model d)
            , "tags" .= n_tags d]

finalizeNote :: Note -> UUIDGen Note
finalizeNote n = do
    nm <- finalizeNoteModel $ n_note_model n
    return $
        n
        { n_note_model = nm
        , n_guid = getUUIDfromString (show n)
        }
