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

data Concept = Concept
  { getID              :: String
  , getUID             :: Integer
  , getName            :: String
  , getHeadingMeta     :: HeadingMeta
  , getTitle           :: [Inline]
  , getConceptContents :: [Block]
  } deriving (Ord, Eq, Generic)

instance ToJSON Concept

type ConceptTree = Tree Concept

-- https://tikzcd.yichuanshen.de/#N4Igdg9gJgpgziAXAbVABwnAlgFyxMJZARgBoAGAXVJADcBDAGwFcYkQA5AAgAoAdPjhgAPHACcAtsADKMAMZ4CXABYx6UAL4BKEBtLpMufIRTlSxanSat2AYV4ChoyTPmKwXPDkYxtu-SAY2O5EZBY0DCxsiJwOgiLiUj70AGZ+egbBxkQATOaWkTYx3PzxzklqaToZgYYhpqQ5BdbRIPalTonAtvRimtUBQUYERADMjc1RdnGdLj196YN12ShkTREt7MiU-pnDJsh561ZTMdu6ljBQAObwRKApYhASSGYgOBBIxDWPz680HyQOR+TxeiDegMQoxBf0QeXen0QABYYWDxgikABWVFAgGIgBsOKheKQAHYNJQNEA
type ConceptForest = [ConceptTree]

data InternalDeck = ID
  { getDeckName    :: String
  , getAuthor      :: String
  , getCardLevel   :: Integer
  , getConceptTree :: ConceptTree
  , otherMeta      :: String -> Maybe String
  }

data RenderOptions = RO
  { wantsHtml  :: Bool
  , wantsLatex :: Bool
  , getHeader  :: String
  }

instance ToJSON InternalDeck where
  toJSON d =
    object
      [ "title" .= getDeckName d
      , "author" .= getAuthor d
      , "root" .= toJSON (getConceptTree d)
      ]

blockToHTML :: [Block] -> String
blockToHTML b = writeHtmlString def (Pandoc (Meta Map.empty) b)

type Chunk = (Block, [Block])

getLevel :: Chunk -> Int
getLevel (Header n _ _, _) = n
getLevel _                 = error "Sorry, chunk is invalid"

add :: Block -> [Chunk] -> [Chunk]
add x@Header {} cs = cs ++ [(x, [])]
add x cs@(_:_) =
  let i = init cs
  -- Concept tree displayed here:
      (h, l) = last cs
  in i ++ [(h, l ++ [x])]
add x [] = []

-- This might happen also when some meta info is present but with a blank value
-- Pandoc recognizes this as a paragraph and so it appears as some info outside concepts
-- final decision; change from:
-- error "Sorry, the document should begin with at least one top concept"
-- to
asDasherized :: [Inline] -> [Char]
asDasherized = concatMap dasherize

-- [Str \"Generator\",Space,Str \"set\",Space,Str \"(Monoid)\",Span (\"\",[\"tag\"],[(\"data-tag-name\",\"noanki\")]) []]
getTags :: [Inline] -> [(String, String)]
getTags ((Span ("", ["tag"], pls) _):pps) = getTags' pls ++ getTags pps
  where
    getTags' []                        = []
    getTags' (("data-tag-name", s):as) = [(s, "true")] ++ getTags' as
    getTags' (_:as)                    = getTags' as
getTags (_:pps) = getTags pps
getTags [] = []

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
--    ([Chunk] -> Maybe (ConceptTree, [Chunk]))
--
toInternalDeckStruct :: Int -> [Chunk] -> Maybe (ConceptTree, [Chunk])
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
               (Map.fromList (mt ++ getTags text))
               text
               bs)
            subFor
    in Just (currentTree, remaining)
  | n0 /= n1 = Nothing
toInternalDeckStruct _ [] = Nothing

chunksToConceptTree :: [Chunk] -> ConceptTree
chunksToConceptTree cs =
  let subFor = unfoldr (toInternalDeckStruct 1) cs
      currentTree = Node (Concept "root" 0 "Root" Map.empty [] []) subFor
  in currentTree

getChunks :: Pandoc -> [Chunk]
getChunks (Pandoc _ bs) = foldl (flip add) [] bs

parseDeck :: Pandoc -> InternalDeck
parseDeck x@(Pandoc meta _) =
  ID
    (gm "title")
    (gm "author")
    (read $ fromMaybe "1" (getMV "level"))
    (chunksToConceptTree $ getChunks x)
    getMV
  where
    gm k = fromMaybe "NA" $ getMV k
    getMV k =
      case Map.lookup k (unMeta meta) of
        Just (MetaInlines mi) -> Just $ asPlainString mi
        _                     -> Nothing

singletonTree :: Concept -> ConceptTree
singletonTree a = Node a []

nemptyLeaf :: Concept -> Bool
nemptyLeaf s = length (getConceptContents s) > 0

root xs = Node (Concept "root" 0 "Root" Map.empty [] []) $ xs

forestOfSingletons :: [Concept] -> ConceptForest
forestOfSingletons as = cards
  where
    cards = map singletonTree $ (filter nemptyLeaf as)

flattenTreeLev :: Integer -> ConceptTree -> ConceptTree
flattenTreeLev 0 t@(Node x cs) = (Node x (forestOfSingletons $ flatten t))
flattenTreeLev n (Node x cs)   = (Node x (map (flattenTreeLev (n - 1)) cs))

flattenDeckLev :: InternalDeck -> Integer -> InternalDeck
flattenDeckLev ideck n =
  ideck
  { getConceptTree = (flattenTreeLev n (getConceptTree ideck))
  , getCardLevel = (2 - n + 1)
  }

has :: String -> Concept -> Bool
has s q =
  case (Map.lookup s (getHeadingMeta q)) of
    Just _  -> True
    Nothing -> False

deepHasFocus (Node c ns) = (has "focus" c) || any deepHasFocus ns

removeSubtrees :: ConceptTree -> ConceptTree
removeSubtrees n@(Node c ns) =
  if has "focus" c
    then (Node c ns)
    else Node c (filter deepHasFocus (map removeSubtrees ns))

removeNoAnkiNodes :: InternalDeck -> InternalDeck
removeNoAnkiNodes ideck =
  ideck {getConceptTree = removeSubtrees (getConceptTree ideck)}

parseOrg :: String -> InternalDeck
parseOrg f = removeNoAnkiNodes $ parseDeck $ readDoc f

unLines :: String -> String
unLines s = intercalate "" $ splitOn "\n" s

renderLeaf :: Concept -> String
renderLeaf l = unLines (blockToHTML . getConceptContents $ l)

showDeck :: InternalDeck -> String
showDeck d = drawTree $ fmap showSL (getConceptTree d)

showSL :: Concept -> String
showSL s = getName s ++ ": " ++ renderLeaf s ++ (show $ getHeadingMeta s)

_drawAsForest :: String -> String
_drawAsForest f =
  let d = parseDeck $ readDoc f
      author = getAuthor d
      getTitle = getDeckName d
      content = showDeck d
      l = show (getCardLevel d)
  in [i|#{getTitle} - #{author} (encoding cards from level #{l} downwards (Root = 0))
#{content}
       |]

_printAsJson :: String -> String
_printAsJson f =
  let d = parseDeck $ readDoc f
  in BL.unpack $ encodePretty d

renderAsInternal :: InternalDeck -> IO String
renderAsInternal x = return $ showDeck x
