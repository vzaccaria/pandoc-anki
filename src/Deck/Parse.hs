{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Deck.Parse where

import Control.Monad.Supply
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Char (toLower)
import Data.List
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import Data.Maybe
import Data.String.Interpolate
import Data.Tree
import Debug.Trace
import System.Directory
import System.Process
import qualified Data.Text as T
import Text.Pandoc
import Text.Pandoc.Error
import Text.Pandoc.Walk (walk)
import Utils

type HeadingMeta = Map.Map String String

data StructureLeaf = Concept
    { getID :: String
    , getUID :: Integer
    , getName :: String
    , getHeadingMeta :: HeadingMeta
    , contents :: [Block]
    } deriving (Ord,Eq,Generic)

instance ToJSON StructureLeaf

-- instance ToJSON StructureLeaf where
-- toJSON s = String $ T.pack $ processLeafContents (contents s)
type Structure = Tree StructureLeaf

data InternalDeck = ID
    { getTitle :: String
    , getAuthor :: String
    , getCardLevel :: Integer
    , getStructure :: Structure
    , otherMeta :: String -> Maybe String
    }

instance ToJSON InternalDeck where
    toJSON d =
        object
            [ "title" .= getTitle d
            , "author" .= getAuthor d
            , "root" .= toJSON (getStructure d)]

getDeckName :: InternalDeck -> String
getDeckName = getTitle

processLeafContents :: [Block] -> String
processLeafContents b = writeHtmlString def (Pandoc (Meta Map.empty) b)

type Chunk = (Block, [Block])

getLevel :: Chunk -> Int
getLevel (Header n _ _,_) = n
getLevel _ = error "Sorry, chunk is invalid"

add :: Block -> [Chunk] -> [Chunk]
add x@Header{} cs = cs ++ [(x, [])]
add x cs@(_:_) =
    let i = init cs
        (h,l) = last cs
    in i ++ [(h, l ++ [x])]
add x [] =
    -- This might happen also when some meta info is present but with a blank value
    -- Pandoc recognizes this as a paragraph and so it appears as some info outside concepts
    -- final decision; change from:
    -- error "Sorry, the document should begin with at least one top concept"
    -- to
    []

asDasherized :: [Inline] -> [Char]
asDasherized = concatMap dasherize

asPlainString :: [Inline] -> [Char]
asPlainString = concatMap f
  where
    f (Str s) = s
    f Space = " "
    f _ = "*"

-- Need to use unfoldr (anamorphism):
--
--    unfoldr:: ([a] -> Maybe (b, [a])) -> [a] -> [b]
--
-- so we need to build:
--
--    ([Chunk] -> Maybe (Structure, [Chunk]))
--
toStructure
    :: Int -> [Chunk] -> Maybe (Structure, [Chunk])
toStructure n0 ((h@(Header n1 (_,_,mt) text),bs):cs)
  | n0 == n1 =
      let getValidBlocks =
              takeWhile'
                  (\x ->
                        getLevel x > n1)
          (valid,remaining) = getValidBlocks cs
          subFor = unfoldr (toStructure $ n0 + 1) valid
          currentTree =
              Node
                  (Concept
                       (asDasherized text)
                       0
                       (asPlainString text)
                       (Map.fromList mt)
                       bs)
                  subFor
      in Just (currentTree, remaining)
  | n0 /= n1 = Nothing
toStructure _ [] = Nothing

parseChunks :: [Chunk] -> Structure
parseChunks cs =
    let subFor = unfoldr (toStructure 1) cs
        currentTree = Node (Concept "root" 0 "Root" Map.empty []) subFor
    in currentTree

getChunks :: Pandoc -> [Chunk]
getChunks (Pandoc _ bs) = foldl (flip add) [] bs

parseDeck :: Pandoc -> InternalDeck
parseDeck x@(Pandoc meta _) =
    ID
        (gm "title")
        (gm "author")
        (read $ fromMaybe "1" (getMV "level"))
        (parseChunks $ getChunks x)
        getMV
  where
    gm k = fromMaybe "NA" $ getMV k
    getMV k =
        case Map.lookup k (unMeta meta) of
            Just (MetaInlines mi) -> Just $ asPlainString mi
            _ -> Nothing

singletonTree :: StructureLeaf -> Structure
singletonTree a = Node a []

nemptyLeaf :: StructureLeaf -> Bool
nemptyLeaf s = length (contents s) > 0

root xs = Node (Concept "root" 0 "Root" Map.empty []) $ xs

deck xs = Node (Concept "deck" 0 "All cards" Map.empty []) $ xs

listOfSingletons :: [StructureLeaf] -> Structure
listOfSingletons as = root $ [deck cards]
  where
    cards = map singletonTree $ (filter nemptyLeaf as)

flattenTree :: Structure -> Structure
flattenTree t = listOfSingletons (flatten t)

flattenDeck :: InternalDeck -> InternalDeck
flattenDeck ideck =
    ideck
    { getStructure = (flattenTree $ getStructure ideck)
    , getCardLevel = 2
    }