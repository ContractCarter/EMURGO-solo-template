{-# LANGUAGE NamedFieldPuns #-}
module Lib where

import Types
import System.Random

move :: Direction -> Position -> Position
move dir (r, c) = case dir of
    U -> (r - 1, c)
    D -> (r + 1, c)
    L -> (r, c - 1)
    R -> (r, c + 1)

advance :: GameState -> Command -> GameState
advance (Game world@World{snake, food, direction, randomGen, bounds} score) cmd = case cmd of
    Exit -> GameOver score
    Move dir -> if collision then GameOver score else newGame
        where
          newDirection = if dir == opposite direction then direction else dir
          newHead = move newDirection (head snake)
          collision = newHead `elem` snake || outOfBounds bounds newHead
          (newSnake, newFood, newRand) = if newHead == food
                                         then (newHead : snake, randomPosition bounds randomGen, snd $ next randomGen)
                                         else (newHead : init snake, food, randomGen)
          newGame = Game (world {snake = newSnake, food = newFood, direction = newDirection, randomGen = newRand}) (score + length newSnake)
advance game _ = game

opposite :: Direction -> Direction
opposite dir = toEnum $ (fromEnum dir + 2) `mod` 4

randomPosition :: (Int, Int) -> StdGen -> Position
randomPosition (maxr, maxc) g = 
    let (r, g1) = randomR (1, maxr) g
        (c, _) = randomR (1, maxc) g1
    in (r, c)

outOfBounds :: (Int, Int) -> Position -> Bool
outOfBounds (maxr, maxc) (r, c) = r < 1 || r > maxr || c < 1 || c > maxc

initWorld :: StdGen -> World
initWorld g = World { snake = [(10, x)| x <- [20,19..15]],
                      food = (10, 10),
                      direction = R,
                      randomGen = g,
                      bounds = (20, 40)
                    }

initGame :: StdGen -> GameState
initGame = Game <$> initWorld


parseCommand :: Char -> Maybe Command
parseCommand ch = case ch of
    'q' -> Just Exit
    'w' -> Just $ Move U
    'a' -> Just $ Move L
    's' -> Just $ Move D
    'd' -> Just $ Move R
    _   -> Nothing
