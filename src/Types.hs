module Types where

import System.Random


data Direction = Up | Down | Left | Right deriving (Eq, Enum)


data Command = Exit | Move Direction deriving (Eq)


type Position = (Int, Int)


type Snake = [Position]


data World = World { 
                    snake :: Snake,
                    food :: Position,
                    direction :: Direction,
                    randomGen :: StdGen,
                    bounds :: (Int, Int)
                   }


data GameState = Game { world :: World, score :: Int } | GameOver Int
