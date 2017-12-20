{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Deck.Internal.Render where

import           Deck.RenderCommon

render :: String -> String
render = _printAsJson
