{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Backend.CrowdAnki.NoteModel where

import           Data.Aeson
import qualified Data.ByteString.Lazy    as BL
import           Data.Default
import           Data.List
import           Data.List.Split
import qualified Data.Map                as Map
import           Data.String.Interpolate
import           Data.Tree
import           GHC.Exts
import           GHC.Generics
import           Text.Pandoc
import           Text.Pandoc.Walk        (walk)
import           Utils

data NoteModel = NM
    { nm_css       :: String
    , nm_flds      :: [NoteModelField]
    , nm_latexPost :: String
    , nm_latexPre  :: String
    , nm_name      :: String
    , nm_sortf     :: Integer
    , nm_tags      :: [String]
    , nm_tmpls     :: [NoteTmpl]
    , nm_uuid      :: String
    } deriving (Show,Generic,Eq)

data NoteTmpl = NT
    { nt_questionFormat :: String
    , nt_answerFormat   :: String
    , nt_ord            :: Integer
    , nt_name           :: String
    , nt_bFont          :: String
    , nt_bSize          :: Integer
    , nt_bqfmt          :: String
    , nt_bafmt          :: String
    } deriving (Show,Generic,Eq)

data NoteModelField = NMF
    { nmf_font   :: String
    , nmf_media  :: [String]
    , nmf_name   :: String
    , nmf_ord    :: Integer
    , nmf_rtl    :: Bool
    , nmf_size   :: Integer
    , nmf_sticky :: Bool
    } deriving (Show,Generic,Eq)

nmf_def_front = NMF "Arial" [] "Front" 0 False 20 False

nmf_def_back = NMF "Arial" [] "Back" 1 False 20 False

reqAll =
    Array $
    fromList
        [Array $ fromList [Number 0, String "all", Array $ fromList [Number 0]]]

leCSS =
    [i|

|]

tikzLatex =
    [i| \\documentclass[10pt]{article}
\\usepackage[paperheight=12cm,paperwidth=11cm,margin=0.5cm]{geometry}

\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{longtable}
\\usepackage{booktabs}

\\usepackage{libertine-type1}
\\usepackage{biolinum-type1}
\\usepackage{libertineMono-type1}
\\usepackage[libertine]{newtxmath}

\\usepackage{amssymb,amsmath}
\\usepackage{xcolor}
\\usepackage{tikz-cd}

\\usepackage{inconsolata}
\\newcommand{\\kk}{\\mathbb{k}}
\\newcommand{\\KK}{\\mathbb{K}}
\\newcommand{\\CC}{\\mathbb{C}}
\\newcommand{\\BB}{\\mathbb{B}}
\\newcommand{\\NN}{\\mathbb{N}}
\\newcommand{\\RR}{\\mathbb{R}}
\\newcommand{\\np}[2]{\\langle #1, #2 \\rangle}
\\newcommand{\\ang}[1]{\\langle #1 \\rangle}
\\newcommand{\\ladjof}{\\dashv}
\\newcommand{\\radjof}{\\vdash}
\\newcommand{\\ds}{d^*}
\\newcommand{\\es}{e^*}
\\newcommand{\\fs}{f^*}


\\pagestyle{empty}

\\providecommand{\\tightlist}{%
  \\setlength{\\itemsep}{0pt}\\setlength{\\parskip}{0pt}}


\\begin{document}
|]

instance Default NoteModel where
    def =
        NM
            leCSS
            [nmf_def_front, nmf_def_back]
            "\\end{document}"
            tikzLatex
            "DefaultName"
            0
            []
            [(def :: NoteTmpl)]
            "NoUID"

instance Default NoteTmpl where
    def =
        NT
            "{{Front}}"
            "{{FrontSide}}\n\n<hr id=answer>\n\n{{Back}}"
            0
            "Card 1"
            "Arial"
            12
            ""
            ""

instance ToJSON NoteModel where
    toJSON d =
        object
            [ "__type__" .= ("NoteModel" :: String)
            , "crowdanki_uuid" .= nm_uuid d
            , "css" .= nm_css d
            , "flds" .= nm_flds d
            , "latexPost" .= nm_latexPost d
            , "latexPre" .= nm_latexPre d
            , "req" .= reqAll
            , "name" .= nm_name d
            , "sortf" .= nm_sortf d
            , "tags" .= nm_tags d
            , "tmpls" .= nm_tmpls d
            , "type" .= (0 :: Integer)
            , "vers" .= ([] :: [Integer])]

instance ToJSON NoteTmpl where
    toJSON d =
        object
            [ "afmt" .= nt_answerFormat d
            , "qfmt" .= nt_questionFormat d
            , "name" .= nt_name d
            , "ord" .= nt_ord d
            , "did" .= Data.Aeson.Null
            , "bqfmt" .= nt_bqfmt d
            , "bafmt" .= nt_bafmt d
            , "bfont" .= nt_bFont d
            , "bsize" .= nt_bSize d]

instance ToJSON NoteModelField where
    toJSON d =
        object
            [ "font" .= nmf_font d
            , "media" .= nmf_media d
            , "name" .= nmf_name d
            , "ord" .= nmf_ord d
            , "rtl" .= nmf_rtl d
            , "size" .= nmf_size d
            , "sticky" .= nmf_sticky d]

finalizeNoteModel :: NoteModel -> UUIDGen NoteModel
finalizeNoteModel nm =
    return
        (nm
         { nm_uuid = getUUIDfromString (show nm)
         , nm_name = ("noteModel-" ++ (getUUIDfromString (show nm)))
         })
