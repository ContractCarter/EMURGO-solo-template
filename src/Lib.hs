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
advance (Game world@World{snake, food, direction, randomGen, bounds} score) cmd =
  case cmd of
    Exit -> GameOver score
    Move dir ->
      if any (\seg -> move direction (head snake) == seg) (tail snake) || outOfBounds bounds (move dir (head snake))
        then GameOver score
        else let newSnake = if move dir (head snake) == food
                              then food : snake
                              else init snake
                 (newFood, newRand) = if move dir (head snake) == food
                                        then randomPosition bounds randomGen
                                        else (food, randomGen)
              in Game (world { snake = move dir (head snake) : newSnake
                             , food = newFood
                             , direction = dir
                             , randomGen = newRand
                             }) (score + length newSnake)
advance game _ = game

randomPosition :: (Int, Int) -> StdGen -> (Position, StdGen)
randomPosition (maxr, maxc) g =
  let (r, g1) = randomR (1, maxr) g
      (c, g2) = randomR (1, maxc) g1
  in ((r, c), g2)

outOfBounds :: (Int, Int) -> Position -> Bool
outOfBounds (maxr, maxc) (r, c) = r < 1 || r > maxr || c < 1 || c > maxc

initWorld :: StdGen -> World
initWorld g = World {
  snake = [(10, x) | x <- [20, 19 .. 15]],
  food = (10, 10),
  direction = R,
  randomGen = g,
  bounds = (40, 80)
}

initGame :: StdGen -> GameState
initGame g = Game (initWorld g) 0

parseCommand :: Char -> Maybe Command
parseCommand ch = lookup ch commands
  where
    commands = [('q', Exit), ('w', Move U), ('a', Move L), ('s', Move D), ('d', Move R)]
