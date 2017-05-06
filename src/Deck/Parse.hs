{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.Parse where

import           Control.Monad.Supply
import           Data.Char               (toLower)
import           Data.List
import qualified Data.Map                as Map
import           Data.String.Interpolate
import           Data.Tree
import           Debug.Trace
import           System.Directory
import           System.Process
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.Walk        (walk)
import           Utils

type HeadingMeta = Map.Map String String

data StructureLeaf = Concept
    { getID          :: String
    , getUID         :: Integer
    , getName        :: String
    , getHeadingMeta :: HeadingMeta
    , contents       :: [Block]
    } deriving (Show,Eq)

type Structure = Tree StructureLeaf

data InternalDeck = ID
    { getMapName   :: Maybe String
    , getAuthor    :: Maybe String
    , getStructure :: Structure
    , otherMeta    :: String -> Maybe String
    }

getDeckName :: InternalDeck -> String
getDeckName (ID (Just n) _ _ _) = n
getDeckName (ID _ _ _ _) = "No name"

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
    ID (getMV "title") (getMV "author") (parseChunks $ getChunks x) getMV
  where
    getMV k =
        case Map.lookup k (unMeta meta) of
            Just (MetaInlines mi) -> Just $ asPlainString mi
            _ -> Nothing
