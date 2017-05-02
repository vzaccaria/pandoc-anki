{-# LANGUAGE OverloadedStrings #-}

module Utils where

import           Control.Monad.Identity
import           Control.Monad.Random
import           Data.Char              (ord, toLower)
import qualified Data.Map               as Map
import           Data.UUID
import           Data.UUID.V5
import           Data.Word
import           Text.Pandoc

dasherize :: Inline -> String
dasherize Space = "-"
dasherize (Str s) = map toLower s
dasherize _ = error "Invalid input to dasherize"

readDoc :: String -> Pandoc
readDoc s =
    case readOrg def s of
        Right doc -> doc
        Left err -> error (show err)

writeDoc :: Pandoc -> String
writeDoc = writeLaTeX def

expandToLatex :: [Block] -> String
expandToLatex b = writeDoc $ Pandoc (Meta Map.empty) b

-- A bit like map but stops when f returns Nothing.
takeWhile'
    :: (a -> Bool) -> [a] -> ([a], [a])
takeWhile' f (a:b) =
    case f a of
        True ->
            let r = takeWhile' f b
            in (a : fst r, (snd r))
        False -> ([], a : b)
takeWhile' f [] = ([], [])

charToWord8 :: [Char] -> [Word8]
charToWord8 n = (map (fromIntegral . ord) n) :: [Word8]

getUUIDfromString :: String -> String
getUUIDfromString s = toString $ generateNamed namespaceURL (charToWord8 s)

type UUIDGen a = RandT StdGen Identity a -- Use mkStdGen

getRandomUUID :: UUIDGen String
getRandomUUID =
    getRandom >>=
    \x ->
         return $ toString x
