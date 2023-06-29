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
import Control.Monad 
import Control.Concurrent


main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    clearScreen
    printLogo
    putStrLn "Press any key to start..."
    _ <- getChar
    clearScreen
    commands <- newTQueueIO
    worldVar <- newTVarIO (initGame $ mkStdGen 0)
    withAsync (commandLoop commands) $ \_ -> 
      withAsync (gameLoop commands worldVar) $ \_ ->
        forever (threadDelay 100000)

                                                                                       

   
