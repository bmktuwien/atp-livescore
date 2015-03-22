module Main (main) where

import           AtpLiveScore
import           Control.Applicative
import           Data.Maybe

main :: IO ()
main = do
  resp <- fromJust <$> getScore
  let scores = parseScore resp

  print scores
