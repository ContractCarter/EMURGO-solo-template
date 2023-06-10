module Main where

import Actions
import Lib
import Types


    main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    clearScreen
    printLogo
    commands <- newTQueueIO
    worldVar <- newTVarIO (initGame $ mkStdGen 0)
    withAsync (commandLoop commands) $ \_ -> gameLoop commands worldVar

                                                                                       

   
