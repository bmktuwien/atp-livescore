{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module AtpLiveScore where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import           Data.Char
import qualified Data.HashMap.Strict        as Map
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Tuple
import           Graphics.UI.Gtk.Gdk.Pixbuf
import           Libnotify
import           Network.Http.Client
import           System.Directory
import           System.FilePath
import qualified System.IO.Streams          as Streams
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree
import           Text.Printf
import           Text.Read
import qualified Text.Regex.PCRE.Light      as PCRE

-------------------------------------------------------------------------------

data TourType = ATP | WTA

instance Show TourType where
  show ATP = "atp"
  show WTA = "wta"

instance Read TourType where
  readsPrec _ "atp" = [(ATP, "")]
  readsPrec _ "wta" = [(WTA, "")]
  readsPrec _ _     = error "unknown tour type"

data Settings = Settings
    { settingsPlayerRegex :: Maybe B.ByteString
    , settingsMatchRegex  :: Maybe B.ByteString
    , settingsTourType    :: Maybe TourType
    , settingsRefresh     :: !Int
    , settingsImgDir      :: Maybe FilePath
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

type PlayerImgMap = [(String, Pixbuf)]

-------------------------------------------------------------------------------

startTicker :: Settings -> IO ()
startTicker Settings{..} = do
  imgMap <- maybe (return []) loadImages settingsImgDir
  tickerLoop imgMap Map.empty
  where
    tickerLoop imgMap scoreMap = do
      mResp <- getScore settingsTourType

      let scoreMap' = fromScores $ maybe [] parseScores mResp
          scoreMap'' = mergeScoreMaps scoreMap scoreMap'
          scores = filterPlayers settingsPlayerRegex .
                   filterMatches settingsMatchRegex
                   $ Map.elems scoreMap''

      mapM_ (notifyScore imgMap) scores

      threadDelay (settingsRefresh * 1000000)

      -- update the showed scores and continue
      tickerLoop imgMap $ Map.map (\s -> s { scoreShowed = True }) scoreMap''

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

    filterPlayers Nothing      = id
    filterPlayers (Just regex) = filter f
      where
        f Score{..} = regexMatches regex (playerName scorePlayer1) ||
                      regexMatches regex (playerName scorePlayer2)

    filterMatches Nothing      = id
    filterMatches (Just regex) = filter f
      where
        f Score{..} = regexMatches regex scoreMatch

-- | Retrieves the score from 'tennislive.at' server.
-- The response is an HTML document which should be parsed by 'parseScore'.
getScore :: Maybe TourType -> IO (Maybe B.ByteString)
getScore mType =
  bracket
  (openConnection "www.tennislive.at" 80) closeConnection $ \conn -> do
    ts <- getPOSIXTime
    let currentTS = (round $ ts * 100) :: Int
        typeQuery = maybe "" (("&type=" ++) . show) mType

    request <- buildRequest $ do
      http GET (B8.pack $ "/tennis_livescore.php?t=live" ++ typeQuery ++
                "&" ++ show currentTS)
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

      let set1 = fromMaybe 0 $ lookupGame =<< subtrees !!? (i+2)
          set2 = fromMaybe 0 $ lookupGame =<< subtrees !!? (i+3)
          set3 = fromMaybe 0 $ lookupGame =<< subtrees !!? (i+4)
          set4 = fromMaybe 0 $ lookupGame =<< subtrees !!? (i+5)
          set5 = fromMaybe 0 $ lookupGame =<< subtrees !!? (i+6)
          currentGame = fromMaybe 0 $ lookupPoint =<< subtrees !!? (i+7)

      return $ Player name [set1, set2, set3, set4, set5] isServer currentGame
    extractPlayer _ _ = Nothing

notifyScore :: PlayerImgMap -> Score -> IO ()
notifyScore imgMap Score{..} =
  unless scoreShowed $ display_ $
       summary (printf "%s" $ B8.unpack scoreMatch)
    <> body (formatPlayer scorePlayer1 ++
             formatPlayer scorePlayer2)
    <> playerImg
    <> timeout Default
  where
    playerImg = do
      let mImg1 = lookupPlayerImg imgMap $ playerName scorePlayer1
          mImg2 = lookupPlayerImg imgMap $ playerName scorePlayer2

      case (mImg1,mImg2) of
       (Just pixBuf1, Just pixBuf2) -> if player1IsLeading
                                       then image pixBuf1
                                       else image pixBuf2
       (Just pixBuf,_) -> image pixBuf
       (_,Just pixBuf) -> image pixBuf
       _               -> icon "dialog-information"
      where
        player1IsLeading = scorePlayer1 `isLeading` scorePlayer2

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
   printf "%d | %d | %d | %d | %d || %s   %s %s \n"
   s1 s2 s3 s4 s5 (showGame playerCurrentGame) name server

  where
    showGame :: Int -> String
    showGame g
      | g <= 40   = printf "%.2d" g
      | otherwise = printf "%2s" ("A" :: String)

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
lookupText (TagLeaf (TagText str))  = Just str
lookupText (TagLeaf _)              = Nothing
lookupText (TagBranch _ _ subtrees) =
  listToMaybe $ mapMaybe lookupText subtrees

lookupGame :: TagTree B.ByteString -> Maybe Int
lookupGame = maybe Nothing (readMaybe . B8.unpack) . lookupText

lookupPoint :: TagTree B.ByteString -> Maybe Int
lookupPoint t = case lookupText t of
                  Nothing   -> Nothing
                  Just "A"  -> Just 45
                  Just text -> readMaybe . B8.unpack $ text

containsImage :: TagTree B.ByteString -> Bool
containsImage (TagLeaf _) = False
containsImage (TagBranch str _ subtrees)
  | str == "img" = True
  | otherwise    = any containsImage subtrees

regexMatches :: B.ByteString -> B.ByteString -> Bool
regexMatches rgxStr str = isJust $ PCRE.match regex str []
  where
    regex = PCRE.compile rgxStr []

readDirectory :: FilePath -> IO [FilePath]
readDirectory dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`notElem` [".", ".."]) names

  paths <- forM properNames $ \name -> do
    let path = dir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then readDirectory path
      else return [path]

  return (concat paths)

loadImages :: FilePath -> IO PlayerImgMap
loadImages imgDir = do
  paths <- readDirectory imgDir

  forM paths $ \path -> do
    let playerName = dropExtensions . takeFileName $ path

    pixbuf <- pixbufNewFromFile path

    return (lower playerName, pixbuf)

lookupPlayerImg :: PlayerImgMap -> B.ByteString -> Maybe Pixbuf
lookupPlayerImg [] _ = Nothing
lookupPlayerImg ((n,pb):xs) player
  | regexMatches n' player' = Just pb
  | otherwise = lookupPlayerImg xs player
  where
    n' = B8.pack n
    player' = B8.pack . replaceSpace . lower . B8.unpack $ player

    replaceSpace = map f
      where
        f ' ' = '_'
        f c   = c

isLeading :: Player -> Player -> Bool
player1 `isLeading` player2 =
  (setsP1 > setsP2) ||
  (setsP1 == setsP2 && curSetP1 > curSetP2) ||
  (setsP1 == setsP2 && curSetP1 == curSetP2 && curGameP1 > curGameP2)
  where
    sets = zip (playerSets player1) (playerSets player2)
    finishedSets = filter isFinished sets

    setsP1 = calcSetsWon finishedSets
    setsP2 = calcSetsWon $ map swap finishedSets

    (curSetP1, curSetP2) = head $ dropWhile isFinished sets

    curGameP1 = playerCurrentGame player1
    curGameP2 = playerCurrentGame player2

    isFinished (7,6) = True
    isFinished (6,7) = True
    isFinished (s1,s2)
      | s1 >= 6 || s2 >= 6 = abs(s1 - s2) > 1
      | otherwise = False

    calcSetsWon = sum . map f
      where
        f :: (Int,Int) -> Int
        f (s1,s2) = if s1 > s2 then 1 else 0

lower :: String -> String
lower = map toLower
