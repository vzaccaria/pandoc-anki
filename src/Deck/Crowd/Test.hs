module Deck.Crowd.Test where

import           Data.Aeson
import qualified Data.ByteString.Lazy     as BL
import           Data.Default
import           Data.List
import           Data.List.Split
import qualified Data.Map                 as Map
import           Data.String.Interpolate
import           Data.Tree
import           Deck.Crowd.Deck
import           Deck.Crowd.Note
import           Deck.Crowd.NoteModel
import           GHC.Generics
import           Text.Pandoc
import           Text.Pandoc.Walk         (walk)
import           Utils

n1 = N ["Q:What is a?", "Just this"] 0 (def :: NoteModel) []

n2 = N ["Q:What is b?", "Just this"] 0 (def :: NoteModel) []

d1 =
    (def :: Deck)
    { d_notes = [n1]
    , d_name = "Deck 1"
    , d_desc = "Desc. Deck 1"
    }

d2 =
    (def :: Deck)
    { d_notes = [n2]
    , d_name = "Deck 1"
    , d_desc = "Desc. Deck 1"
    }

dr =
    (def :: Deck)
    { d_children = [d1, d2]
    , d_name = "Root::Deck"
    }
