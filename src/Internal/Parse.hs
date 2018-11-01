{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}

module Internal.Parse where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char                  (toLower)
import           Data.List
import           Data.List.Split
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.String.Interpolate
import qualified Data.Text                  as T
import           Data.Tree
import           Debug.Trace
import           GHC.Generics
import           System.Directory
import           System.Process
import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.Walk           (walk)
import           Utils

type HeadingMeta = Map.Map String String

data InternalDeckStructLeaf = Concept
  { getID          :: String
  , getUID         :: Integer
  , getName        :: String
  , getHeadingMeta :: HeadingMeta
  , contents       :: [Block]
  } deriving (Ord, Eq, Generic)

instance ToJSON InternalDeckStructLeaf

-- instance ToJSON InternalDeckStructLeaf where
-- toJSON s = String $ T.pack $ processLeafContents (contents s)
type InternalDeckStruct = Tree InternalDeckStructLeaf

data InternalDeck = ID
  { getTitle              :: String
  , getAuthor             :: String
  , getCardLevel          :: Integer
  , getInternalDeckStruct :: InternalDeckStruct
  , otherMeta             :: String -> Maybe String
  }

instance ToJSON InternalDeck where
  toJSON d =
    object
      [ "title" .= getTitle d
      , "author" .= getAuthor d
      , "root" .= toJSON (getInternalDeckStruct d)
      ]

getDeckName :: InternalDeck -> String
getDeckName = getTitle

processLeafContents :: [Block] -> String
processLeafContents b = writeHtmlString def (Pandoc (Meta Map.empty) b)

type Chunk = (Block, [Block])

getLevel :: Chunk -> Int
getLevel (Header n _ _, _) = n
getLevel _                 = error "Sorry, chunk is invalid"

add :: Block -> [Chunk] -> [Chunk]
add x@Header {} cs = cs ++ [(x, [])]
add x cs@(_:_) =
  let i = init cs
      (h, l) = last cs
  in i ++ [(h, l ++ [x])]
add x []
    -- This might happen also when some meta info is present but with a blank value
    -- Pandoc recognizes this as a paragraph and so it appears as some info outside concepts
    -- final decision; change from:
    -- error "Sorry, the document should begin with at least one top concept"
    -- to
 = []

asDasherized :: [Inline] -> [Char]
asDasherized = concatMap dasherize

asPlainString :: [Inline] -> [Char]
asPlainString = concatMap f
  where
    f (Str s) = s
    f Space   = " "
    f _       = "*"

-- Need to use unfoldr (anamorphism):
--
--    unfoldr:: ([a] -> Maybe (b, [a])) -> [a] -> [b]
--
-- so we need to build:
--
--    ([Chunk] -> Maybe (InternalDeckStruct, [Chunk]))
--
toInternalDeckStruct :: Int -> [Chunk] -> Maybe (InternalDeckStruct, [Chunk])
toInternalDeckStruct n0 ((h@(Header n1 (_, _, mt) text), bs):cs)
  | n0 == n1 =
    let getValidBlocks = takeWhile' (\x -> getLevel x > n1)
        (valid, remaining) = getValidBlocks cs
        subFor = unfoldr (toInternalDeckStruct $ n0 + 1) valid
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
toInternalDeckStruct _ [] = Nothing

parseChunks :: [Chunk] -> InternalDeckStruct
parseChunks cs =
  let subFor = unfoldr (toInternalDeckStruct 1) cs
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
        _                     -> Nothing

singletonTree :: InternalDeckStructLeaf -> InternalDeckStruct
singletonTree a = Node a []

nemptyLeaf :: InternalDeckStructLeaf -> Bool
nemptyLeaf s = length (contents s) > 0

root xs = Node (Concept "root" 0 "Root" Map.empty []) $ xs

deck xs = Node (Concept "deck" 0 "All cards" Map.empty []) $ xs

listOfSingletons :: [InternalDeckStructLeaf] -> InternalDeckStruct
listOfSingletons as = root $ [deck cards]
  where
    cards = map singletonTree $ (filter nemptyLeaf as)

flattenTree :: InternalDeckStruct -> InternalDeckStruct
flattenTree t = listOfSingletons (flatten t)

flattenDeck :: InternalDeck -> InternalDeck
flattenDeck ideck =
  ideck
  { getInternalDeckStruct = (flattenTree $ getInternalDeckStruct ideck)
  , getCardLevel = 2
  }

parseOrg :: String -> InternalDeck
parseOrg f = parseDeck $ readDoc f

unLines :: String -> String
unLines s = intercalate "" $ splitOn "\n" s

renderLeaf :: InternalDeckStructLeaf -> String
renderLeaf l = unLines (processLeafContents . contents $ l)

showDeck :: InternalDeck -> String
showDeck d = drawTree $ fmap showSL (getInternalDeckStruct d)

showSL :: InternalDeckStructLeaf -> String
showSL s = getName s ++ ": " ++ renderLeaf s

_drawAsForest :: String -> String
_drawAsForest f =
  let d = parseDeck $ readDoc f
      author = getAuthor d
      title = getTitle d
      content = showDeck d
      l = show (getCardLevel d)
  in [i|#{title} - #{author} (encoding cards from level #{l} downwards (Root = 0))
#{content}
       |]

_printAsJson :: String -> String
_printAsJson f =
  let d = parseDeck $ readDoc f
  in BL.unpack $ encodePretty d

renderAsInternal :: InternalDeck -> IO String
renderAsInternal x = return $ showDeck x
