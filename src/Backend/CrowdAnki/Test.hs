module Backend.CrowdAnki.Test where

import           Data.Aeson
import qualified Data.ByteString.Lazy     as BL
import           Data.Default
import           Data.List
import           Data.List.Split
import qualified Data.Map                 as Map
import           Data.String.Interpolate
import           Data.Tree
import           Backend.CrowdAnki.Deck
import           Backend.CrowdAnki.Note
import           Backend.CrowdAnki.NoteModel
import           GHC.Generics
import           Text.Pandoc
import           Text.Pandoc.Walk         (walk)
import           Utils

n1 :: Note
n1 = N ["Q:What is a?", "Just this"] 0 (def :: NoteModel) [] ""

n2 :: Note
n2 = N ["Q:What is b?", "Just this"] 0 (def :: NoteModel) [] ""

d2 :: Deck
d2 =
    (def :: Deck)
    { d_notes = [n1]
    , d_name = "Deck 1"
    }

d1 :: Deck
d1 =
    (def :: Deck)
    { d_notes = [n2]
    , d_name = "Deck 1"
    }

dr :: Deck
dr =
    (def :: Deck)
    { d_children = [d1, d2]
    , d_name = "Cippa Lippa"
    }
