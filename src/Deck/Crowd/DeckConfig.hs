{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.Crowd.DeckConfig where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List
import           Data.List.Split
import qualified Data.Map                   as Map
import           Data.String.Interpolate
import           Data.Tree
import           Deck.Crowd.Note
import           Deck.Crowd.NoteModel
import           GHC.Generics
import           Text.Pandoc
import           Text.Pandoc.Walk           (walk)
import           Utils

data DeckConfig =
    Base deriving (Show)

dc_uuid :: DeckConfig -> String
dc_uuid d = "a69dd698-6d5b-11e6-adf3-8c705a50cbf0"

instance ToJSON DeckConfig where
    toJSON Base =
        object
            [ "__type__" .= ("DeckConfig" :: String)
            , "autoplay" .= True
            , "crowdanki_uuid" .= dc_uuid Base
            , "dyn" .= False]
