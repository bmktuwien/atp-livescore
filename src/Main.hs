module Main (main) where

import           AtpLiveScore

main :: IO ()
main = do
  let defaultSettings = Settings Nothing Nothing
  startTicker defaultSettings
