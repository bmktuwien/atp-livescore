{-# LANGUAGE OverloadedStrings #-}

module AtpLiveScore where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Time.Clock.POSIX
import           Network.Http.Client
import qualified System.IO.Streams     as Streams

-- "http://www.tennislive.at/tennis_livescore.php?t=np&142702774607"

update :: IO ()
update = do
  connection <- openConnection "www.tennislive.at" 80

  ts <- getPOSIXTime
  let currentTS = round $ ts * 100

  request <- buildRequest $ do
    http GET (B8.pack $ "/tennis_livescore.php?t=np&" ++ show ts)
    setAccept "text/html"

  sendRequest connection request emptyBody

  receiveResponse connection $ \p is -> do
    mBody <- Streams.read is
    case mBody of
     Just body -> print body
     Nothing   -> return ()

  closeConnection connection
