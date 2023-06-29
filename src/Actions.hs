{-# LANGUAGE NamedFieldPuns #-}
module Actions where

import System.IO
import System.Console.ANSI
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Types
import Lib

printLogo :: IO ()
printLogo = do
    putStrLn "                        /^\\/^\\                                       "
    putStrLn "                      _|__|  O|                                      "
    putStrLn "             \\/     /~     \\_/ \\                                     "
    putStrLn "              \\____|__________/  \\                                   "
    putStrLn "                     \\_______      \\                                 "
    putStrLn "                             `\\     \\                                "
    putStrLn "                               |     |                               "                  
    putStrLn "       ,--./,-.                {     }                               "
    putStrLn "      /# SNAKE  \\              {     }                               "
    putStrLn "     |   GAME   |              {     }                               "         
    putStrLn "      \\        /               |     |                               "
    putStrLn "       `._,._,'               /     /                    \\           "
    putStrLn "                             /     /                       \\\\        "
    putStrLn "                           /      /                         \\ \\      "
    putStrLn "                          /     /                            \\  \\    "
    putStrLn "                         /     /             _----_            \\   \\ "
    putStrLn "                        /     /           _-~      ~-_         |   | "
    putStrLn "                       (      (        _-~    _--_    ~-_     _/   | "
    putStrLn "                        \\      ~-____-~    _-~    ~-_    ~-_-~    /  "
    putStrLn "                         ~-_           _-~          ~-_       _-~    "
    putStrLn "                            ~--______-~                ~-___-~       "   
    putStrLn "                                                                     "


    
    
commandLoop :: TQueue Command -> IO ()
commandLoop commands = forever $ do
    cmd <- parseCommand <$> getChar
    maybe (return ()) (atomically . writeTQueue commands) cmd


gameLoop :: TQueue Command -> TVar GameState -> IO ()
gameLoop commands worldVar = loop where
    loop = do
        gameState <- readTVarIO worldVar
        maybeCmd <- atomically $ tryReadTQueue commands
        let cmd = case gameState of
              Game world _ -> maybe (Move $ direction world) id maybeCmd
              GameOver _ -> maybe Exit id maybeCmd
        let newWorld = advance gameState cmd
        atomically $ writeTVar worldVar newWorld
        clearScreen
        renderWorld newWorld
        case newWorld of
            GameOver score -> putStrLn $ "Game Over! Your score: " ++ show score
            _ -> threadDelay 100000 >> loop


renderWorld :: GameState -> IO ()
renderWorld (Game World{snake, food} score) = do
    putStrLn $ "Score: " ++ show score
    forM_ snake $ \(x, y) -> do
        setCursorPosition x y
        putChar '*'
    let (fx, fy) = food
    setCursorPosition fx fy
    putChar '#'
renderWorld (GameOver _) = return ()
   
