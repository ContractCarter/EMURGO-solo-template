{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Actions
import Lib
import Types
import System.IO
import System.Console.ANSI
import System.Random
import Control.Concurrent.STM
import Control.Concurrent.Async

    main :: IO ()
    main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    clearScreen
    printLogo
    commands <- newTQueueIO
    worldVar <- newTVarIO (initGame $ mkStdGen 0)
    withAsync (commandLoop commands) $ \_ -> gameLoop commands worldVar

                                                                                       

   
