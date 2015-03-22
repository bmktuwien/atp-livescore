{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AtpLiveScore where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.Char
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Libnotify
import           Network.Http.Client
import qualified System.IO.Streams      as Streams
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.Printf
import           Text.Read

-------------------------------------------------------------------------------

data Player = Player
    { playerName        :: !B.ByteString
    , playerSets        :: [Int]
    , playerCurrentGame :: !Int
    } deriving (Eq, Show)

data Score = Score
    { scoreMatch   :: !B.ByteString
    , scorePlayer1 :: !Player
    , scorePlayer2 :: !Player
    } deriving (Eq, Show)

-------------------------------------------------------------------------------
startTicker :: IO ()
startTicker = tickerLoop
  where
    tickerLoop = do
      mResp <- getScore

      case mResp of
       Nothing   -> return ()
       Just resp -> do
         let scores = parseScores resp

         mapM_ notifyScore scores

      -- wait for 5 sec
      threadDelay 10000000

      tickerLoop

-- | Retrieves the score from 'tennislive.at' server.
-- The response is an HTML document which should be parsed by 'parseScore'.
getScore :: IO (Maybe B.ByteString)
getScore =
  bracket
  (openConnection "www.tennislive.at" 80) closeConnection $ \conn -> do
    ts <- getPOSIXTime
    let currentTS = round $ ts * 100

    request <- buildRequest $ do
      http GET (B8.pack $ "/tennis_livescore.php?t=live&" ++ show currentTS)
      setAccept "text/html"

    sendRequest conn request emptyBody

    receiveResponse conn $ \p is -> Streams.read is

-- | Parses the HTML response from the 'tennislive.at' server.
-- This function assumes a certain structure of the response, so if the
-- assumed structure does not match, this function can fail or return
-- non-sensical data.
parseScores :: B.ByteString -> [Score]
parseScores inp = extractScores matchTable
  where
    -- second element in the list is our match table
    matchTable = fromJust . cleanWhiteSpace $
                 (tagTree . parseTags $ inp) !! 1

    extractScores (TagBranch _ _ subtrees) = go subtrees
      where
        go [] = []
        go (mn:p1:p2:xs) =
          let matchName = extractMatchName mn
              player1 = extractPlayer1 p1
              player2 = extractPlayer2 p2 in
           Score matchName player1 player2 : go xs

    extractMatchName (TagBranch _ _ (td:_)) =
      fromMaybe "Unknown" $ lookupText td
    extractMatchName _ = "Unknown"

    extractPlayer1 (TagBranch _ _ subtrees) =
      let name = fromMaybe "Unknown" . lookupText $ subtrees !! 1
          set1 = fromMaybe 0 . lookupInt $ subtrees !! 3
          set2 = fromMaybe 0 . lookupInt $ subtrees !! 4
          set3 = fromMaybe 0 . lookupInt $ subtrees !! 5
          set4 = fromMaybe 0 . lookupInt $ subtrees !! 6
          set5 = fromMaybe 0 . lookupInt $ subtrees !! 7
          currentGame = fromMaybe 0 . lookupInt $ subtrees !! 8 in

      Player name [set1, set2, set3, set4, set5] currentGame

    extractPlayer2 (TagBranch _ _ subtrees) =
      let name = fromMaybe "Unknown" . lookupText $ head subtrees
          set1 = fromMaybe 0 . lookupInt $ subtrees !! 2
          set2 = fromMaybe 0 . lookupInt $ subtrees !! 3
          set3 = fromMaybe 0 . lookupInt $ subtrees !! 4
          set4 = fromMaybe 0 . lookupInt $ subtrees !! 5
          set5 = fromMaybe 0 . lookupInt $ subtrees !! 6
          currentGame = fromMaybe 0 . lookupInt $ subtrees !! 7 in

      Player name [set1, set2, set3, set4, set5] currentGame

-------------------------------------------------------------------------------

cleanWhiteSpace :: TagTree B.ByteString -> Maybe (TagTree B.ByteString)
cleanWhiteSpace t@(TagLeaf (TagText str))
      | B8.all isSpace str = Nothing
      | otherwise = Just t
cleanWhiteSpace (TagLeaf tag) = Just (TagLeaf tag)
cleanWhiteSpace (TagBranch str attrs subtrees) =
  Just . TagBranch str attrs . catMaybes $ map cleanWhiteSpace subtrees


lookupText :: TagTree B.ByteString -> Maybe B.ByteString
lookupText (TagLeaf (TagText str)) = Just str
lookupText (TagLeaf tag) = Nothing
lookupText (TagBranch _ _ subtrees) =
  listToMaybe $ mapMaybe lookupText subtrees

lookupInt :: TagTree B.ByteString -> Maybe Int
lookupInt = maybe Nothing (readMaybe . B8.unpack) . lookupText

formatScore :: Score -> String
formatScore Score{..} =
  concat [ printf "Match: %s\n" $ B8.unpack scoreMatch
         , formatPlayer scorePlayer1
         , formatPlayer scorePlayer2
         ]

formatPlayer :: Player -> String
formatPlayer Player{..} =
  let [s1,s2,s3,s4,s5] = playerSets in

  printf "%d | %d | %d | %d | %d || %.2d   (%s) \n"
         s1 s2 s3 s4 s5 playerCurrentGame $ B8.unpack playerName

notifyScore :: Score -> IO ()
notifyScore Score{..} =
  display_ $
       summary (printf "%s" $ B8.unpack scoreMatch)
    <> body (concat [ formatPlayer scorePlayer1
                    , formatPlayer scorePlayer2
                    ])
    <> icon "dialog-information"
    <> timeout Default
