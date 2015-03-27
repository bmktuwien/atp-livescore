{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AtpLiveScore where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import           Data.Char
import qualified Data.HashMap.Strict    as Map
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

data TourType = ATP | WTA

instance Show TourType where
  show ATP = "atp"
  show WTA = "wta"

data Settings = Settings
    { settingsFollowRegex :: Maybe String
    , settingsTourType    :: Maybe TourType
    } deriving (Show)


data Player = Player
    { playerName        :: !B.ByteString
    , playerSets        :: [Int]
    , playerIsServer    :: !Bool
    , playerCurrentGame :: !Int
    } deriving (Eq, Show)

data Score = Score
    { scoreMatch   :: !B.ByteString
    , scorePlayer1 :: !Player
    , scorePlayer2 :: !Player
    , scoreShowed  :: !Bool
    } deriving (Eq, Show)

-------------------------------------------------------------------------------
startTicker :: Settings -> IO ()
startTicker Settings{..} = tickerLoop Map.empty
  where
    tickerLoop scoreMap = do
      mResp <- getScore settingsTourType
      let scoreMap' = fromScores $ maybe [] parseScores mResp
          scoreMap'' = mergeScoreMaps scoreMap scoreMap'

      mapM_ notifyScore $ Map.elems scoreMap''

      -- wait for 10 secs
      threadDelay 10000000

      -- update the showed scores and continue
      tickerLoop $ Map.map (\s -> s { scoreShowed = True }) scoreMap''

    fromScores scores = Map.fromList $ map f scores
      where
        f s@Score{..} = ((scoreMatch,
                          playerName scorePlayer1,
                          playerName scorePlayer2), s)

    mergeScoreMaps = Map.unionWith merge
      where
        merge s1 s2
          | (scorePlayer1 s1 == scorePlayer1 s2) &&
            (scorePlayer2 s1 == scorePlayer2 s2) = s1
          | otherwise = s2

-- | Retrieves the score from 'tennislive.at' server.
-- The response is an HTML document which should be parsed by 'parseScore'.
getScore :: Maybe TourType -> IO (Maybe B.ByteString)
getScore mType =
  bracket
  (openConnection "www.tennislive.at" 80) closeConnection $ \conn -> do
    ts <- getPOSIXTime
    let currentTS = (round $ ts * 100) :: Int
        typeQuery = maybe "" (\t -> "&type=" ++ show t) mType

    request <- buildRequest $ do
      http GET (B8.pack $ "/tennis_livescore.php?t=live" ++ typeQuery ++ "&" ++
                show currentTS)
      setAccept "text/html"

    sendRequest conn request emptyBody

    receiveResponse conn $ \_ is -> Streams.read is

-- | Parses the HTML response from the 'tennislive.at' server.
-- This function assumes a certain structure of the response, so if the
-- assumed structure does not match, this function can fail or return
-- non-sensical data.
parseScores :: B.ByteString -> [Score]
parseScores inp = maybe [] extractScores mMatchTable
  where
    -- second element in the list is our match table
    mMatchTable = cleanWhiteSpace =<<
                  (tagTree . parseTags $ inp) !!? 1

    extractScores (TagLeaf _)              = []
    extractScores (TagBranch _ _ subtrees) = fromMaybe [] $ go subtrees ""
      where
        go [] _ = return []
        go (x:xs) matchName
          | TagBranch "tr" [("class","header")] _ <- x = do
              (p1:p2:xs') <- return xs
              matchName'  <- extractMatchName x
              player1     <- extractPlayer p1 1
              player2     <- extractPlayer p2 0
              scores      <- go xs' matchName'
              return $ Score matchName' player1 player2 False: scores
          | otherwise = do
              (p2:xs') <- return xs
              player1  <- extractPlayer x 1
              player2  <- extractPlayer p2 0
              scores   <- go xs' matchName
              return $ Score matchName player1 player2 False: scores

    extractMatchName (TagBranch _ _ (td:_)) = lookupText td
    extractMatchName _                      = Nothing

    extractPlayer (TagBranch _ _ subtrees) i = do
      name <- lookupText =<< subtrees !!? i
      isServer <- containsImage <$> subtrees !!? i

      let set1 = fromMaybe 0 $ lookupInt  =<< subtrees !!? (i+2)
          set2 = fromMaybe 0 $ lookupInt  =<< subtrees !!? (i+3)
          set3 = fromMaybe 0 $ lookupInt  =<< subtrees !!? (i+4)
          set4 = fromMaybe 0 $ lookupInt  =<< subtrees !!? (i+5)
          set5 = fromMaybe 0 $ lookupInt  =<< subtrees !!? (i+6)
          currentGame = fromMaybe 0 $ lookupInt =<< subtrees !!? (i+7)

      return $ Player name [set1, set2, set3, set4, set5] isServer currentGame
    extractPlayer _ _ = Nothing

-------------------------------------------------------------------------------
-- Various helper functions

(!!?) :: [a] -> Int -> Maybe a
(!!?) ls i
  | i < 0         = Nothing
  | i < length ls = Just $ ls !! i
  | otherwise     = Nothing

cleanWhiteSpace :: TagTree B.ByteString -> Maybe (TagTree B.ByteString)
cleanWhiteSpace t@(TagLeaf (TagText str))
      | B8.all isSpace str = Nothing
      | otherwise = Just t
cleanWhiteSpace (TagLeaf tag) = Just (TagLeaf tag)
cleanWhiteSpace (TagBranch str attrs subtrees) =
  Just . TagBranch str attrs . catMaybes $ map cleanWhiteSpace subtrees

lookupText :: TagTree B.ByteString -> Maybe B.ByteString
lookupText (TagLeaf (TagText str)) = Just str
lookupText (TagLeaf _) = Nothing
lookupText (TagBranch _ _ subtrees) =
  listToMaybe $ mapMaybe lookupText subtrees

lookupInt :: TagTree B.ByteString -> Maybe Int
lookupInt = maybe Nothing (readMaybe . B8.unpack) . lookupText

containsImage :: TagTree B.ByteString -> Bool
containsImage (TagLeaf _) = False
containsImage (TagBranch str _ subtrees)
  | str == "img" = True
  | otherwise    = any containsImage subtrees

formatScore :: Score -> String
formatScore Score{..} =
  concat [ printf "Match: %s\n" $ B8.unpack scoreMatch
         , formatPlayer scorePlayer1
         , formatPlayer scorePlayer2
         ]

formatPlayer :: Player -> String
formatPlayer Player{..} =
  let [s1,s2,s3,s4,s5] = playerSets
      name = B8.unpack playerName
      server = (if playerIsServer then "*" else " ") :: String
  in

  printf "%d | %d | %d | %d | %d || %.2d   %s %s \n"
         s1 s2 s3 s4 s5 playerCurrentGame name server

notifyScore :: Score -> IO ()
notifyScore Score{..} =
  unless scoreShowed $ display_ $
       summary (printf "%s" $ B8.unpack scoreMatch)
    <> body (formatPlayer scorePlayer1 ++
             formatPlayer scorePlayer2)
    <> icon "dialog-information"
    <> timeout Default
