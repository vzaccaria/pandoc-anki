{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.Crowd.Deck where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List
import           Deck.Crowd.DeckConfig
import           Deck.Crowd.Note
import           Deck.Crowd.NoteModel
import           GHC.Generics
import           Utils

data Deck = D
    { d_notes       :: [Note]
    , d_children    :: [Deck]
    , d_dyn         :: Integer
    , d_extendNew   :: Integer
    , d_extendRev   :: Integer
    , d_media_files :: [String]
    , d_name        :: String
    , d_desc        :: String
    , d_deckconfig  :: DeckConfig
    } deriving (Show,Generic)

d_dump :: Deck -> IO ()
d_dump d = BL.putStrLn $ encodePretty d

d_uuid :: Deck -> String
d_uuid d = getUUIDfromString (d_name d)

d_note_models :: Deck -> [NoteModel]
d_note_models d =
    let nm_uuid' n = nm_uuid $ n_note_model n
        p na nb = nm_uuid' na == nm_uuid' nb
    in map n_note_model $ nubBy p (d_notes d)

instance ToJSON Deck where
    toJSON d =
        object
            [ "__type__" .= ("Deck" :: String)
            , "children" .= d_children d
            , "crowdanki_uuid" .= d_uuid d
            , "deck_config_uuid" .= dc_uuid (d_deckconfig d)
            , "deck_configurations" .= [d_deckconfig d]
            , "desc" .= d_desc d
            , "dyn" .= d_dyn d
            , "extendNew" .= d_extendNew d
            , "extendRev" .= d_extendRev d
            , "media_files" .= d_media_files d
            , "name" .= d_name d
            , "note_models" .= d_note_models d
            , "notes" .= d_notes d]

instance Default Deck where
    def = D [] [] 0 10 50 [] "Unnamed deck" "No description" Base
