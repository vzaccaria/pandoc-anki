{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.Crowd.DeckConfig where

import qualified Control.Monad.Random       as R
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List
import           Data.List.Split
import qualified Data.Map                   as Map
import           Data.String.Interpolate
import           Data.Tree
import           Data.UUID
import           Deck.Crowd.Note
import           Deck.Crowd.NoteModel
import           GHC.Exts
import           GHC.Generics
import           Text.Pandoc
import           Text.Pandoc.Walk           (walk)
import           Utils

data DeckConfig = Base
    { dc_uuid :: String
    , dc_name :: String
    } deriving (Show,Generic,Eq)

instance ToJSON DeckConfig where
    toJSON dc =
        object
            [ "__type__" .= ("DeckConfig" :: String)
            , "autoplay" .= True
            , "crowdanki_uuid" .= dc_uuid dc
            , "dyn" .= False
            , "name" .= (dc_name dc)
            , "lapse" .=
              object
                  [ "delays" .= (Array $ fromList [Number 10])
                  , "leechAction" .= Number 0
                  , "leechFails" .= Number 8
                  , "minInt" .= Number 1
                  , "mult" .= Number 0]
            , "maxTaken" .= Number 60
            , "new" .=
              object
                  [ "bury" .= True
                  , "delays" .= (Array $ fromList [Number 1, Number 10])
                  , "initialFactor" .= Number 2500
                  , "ints" .= (Array $ fromList [Number 1, Number 4, Number 7])
                  , "order" .= Number 1
                  , "perDay" .= Number 20
                  , "separate" .= True]
            , "replayq" .= True
            , "rev" .=
              object
                  [ "bury" .= True
                  , "ease4" .= Number 1.3
                  , "fuzz" .= Number 0.05
                  , "ivlFct" .= Number 1
                  , "maxIvl" .= Number 36500
                  , "minSpace" .= Number 1
                  , "perDay" .= Number 100]
            , "timer" .= Number 0]

finalizeDeckConfig :: DeckConfig -> UUIDGen DeckConfig
finalizeDeckConfig dc = do
    u <- getRandomUUID
    return $
        dc
        { dc_uuid = u
        , dc_name = ("deckConfig-" ++ u)
        }
