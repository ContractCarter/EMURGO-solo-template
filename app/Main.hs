module Main where

import Actions
import Lib
import Types

printLogo :: IO ()
printLogo = do
    putStrLn "                        /^\/^\                                       "
    putStrLn "                      _|__|  O|                                      "
    putStrLn "             \/     /~     \_/ \                                     "
    putStrLn "              \____|__________/  \                                   "
    putStrLn "                     \_______      \                                 "
    putStrLn "                             `\     \                                "
    putStrLn "                               |     |                               "                  
    putStrLn "       ,--./,-.                {     }                               "
    putStrLn "      /# SNAKE  \              {     }                               "
    putStrLn "     |   GAME   |              {     }                               "         
    putStrLn "      \        /               |     |                               "
    putStrLn "       `._,._,'               /     /                    \           "
    putStrLn "                             /     /                       \\        "
    putStrLn "                           /      /                         \ \      "
    putStrLn "                          /     /                            \  \    "
    putStrLn "                         /     /             _----_            \   \ "
    putStrLn "                        /     /           _-~      ~-_         |   | "
    putStrLn "                       (      (        _-~    _--_    ~-_     _/   | "
    putStrLn "                        \      ~-____-~    _-~    ~-_    ~-_-~    /  "
    putStrLn "                         ~-_           _-~          ~-_       _-~    "
    putStrLn "                            ~--______-~                ~-___-~       "   
    putStrLn "                                                                     "
    
    
    main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    clearScreen
    printLogo
    commands <- newTQueueIO
    worldVar <- newTVarIO (initGame $ mkStdGen 0)
    withAsync (commandLoop commands) $ \_ -> gameLoop commands worldVar

                                                                                       

   
