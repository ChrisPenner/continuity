{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Comonad.Cofree
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.Async
import Data.Time.Clock
import Data.Fixed
import Control.Lens hiding ((:<))
import Control.Monad.State
import Data.Monoid
import Data.Foldable
import Control.Comonad
import Control.Applicative

import Continuity.Handler

data St = St { _totalTime :: Double
             , _barChar :: Char
             }
makeLenses ''St

data Event = ChangeChar Char

makePrisms ''Event

countTime :: Handler St Double ()
countTime = do
    timeDelta <- ask
    totalTime += timeDelta

changeChar :: Handler St Event ()
changeChar = liftHandlerOf _ChangeChar $ do
    c <- ask
    barChar .= c

time :: IO a -> IO (Double, a)
time action = do
    currentTime <- getCurrentTime
    a <- action
    endTime <- getCurrentTime
    let timeDiffInSeconds =
            fromRational . toRational
            . nominalDiffTimeToSeconds $ diffUTCTime endTime currentTime
    return (fromRational timeDiffInSeconds, a)

events :: IO (Maybe Event)
events = do
    e <- race (threadDelay 100000) (ChangeChar <$> getChar)
    return $ either (const Nothing) Just e

allHandlers :: Handler St (Double, Maybe Event) ()
allHandlers =
    mconcat [ liftHandlerOf _Just changeChar & argument %~ snd
            , countTime & argument %~ fst
            ]

---

drawBar :: St -> IO ()
drawBar s = putStrLn (replicate (floor pos) (s ^. barChar))
  where
    pos = (1 + sin (s ^. totalTime)) * 20

main :: IO ()
main = continuous (time events) drawBar (buildAnim allHandlers (St 0 '#'))
