{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import qualified Data.ByteString.Char8      as B8
import           Options.Applicative
import           Options.Applicative.Arrows

import           AtpLiveScore

data Args = Args
    { aFollow  :: String
    , aType    :: String
    , aRefresh :: Int
    , aImgDir  :: FilePath
    } deriving (Show)

-------------------------------------------------------------------------------

args :: Parser Args
args = runA $ proc () -> do
  aFollow  <- asA $ strOption (value ""
                               <> short 'f'
                               <> long "follow"
                               <> metavar "REGEX"
                               <> help "Only scores of players are shown, \
                                  \whose names match the regex") -< ()
  aType    <- asA $ strOption (value ""
                               <> short 't'
                               <> long "type"
                               <> help "Only scores of matches of the given tour \
                                  \type are shown. Possible values: atp|wta") -< ()
  aRefresh <- asA $ option (value 10
                            <> short 'r'
                            <> long "refresh-period"
                            <> metavar "SECS"
                            <> help "Time interval in which the data will be fetched"
                            <> showDefault) -< ()
  aImgDir  <- asA $ strOption (value ""
                               <> short 'i'
                               <> long "img-dir"
                               <> metavar "DIR"
                               <> help "Directory with player image files") -< ()
  returnA -< Args {..}

getArgs :: IO Args
getArgs = customExecParser (prefs disambiguate) $
          info (helper <*> args)
          (fullDesc <> header "atp-livescore")

main :: IO ()
main = do
  pargs <- getArgs

  startTicker $ mkSettings pargs

  where
    mkSettings Args{..} =
      let mFollow = if null aFollow then Nothing else Just $ B8.pack aFollow
          mType   = if null aType then Nothing else Just $ read aType
          mImgDir = if null aImgDir then Nothing else Just aImgDir
      in
       Settings mFollow mType aRefresh mImgDir
