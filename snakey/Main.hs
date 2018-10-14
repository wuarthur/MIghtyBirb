module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss

grid, speed :: Float
grid = 20
speed = 1  -- so we move 1 grid space per speed seconds

width, height, offset :: Int
width = 20 * 50 --change the 20 to grid TODO
height = 20 * 50
offset = 100


window :: Display
window = InWindow "Snakey" (width, height) (offset, offset)

background :: Color
background = white

-- Ddescribe the state of the game.
data SnakeGame = Game
  { foodLoc :: (Float, Float)
  , snakeLoc :: (Float, Float)
  , snakeDir :: Char -- N S E W
  } deriving Show


-- | The starting state for the game of Pong.
initialState :: SnakeGame
initialState = Game
  { foodLoc = (-grid, grid*5)
  , snakeLoc = (0, 0)         -- center of the screen
  , snakeDir = 'N'
  }

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [food, snake]
  where
    --  The pong ball.
    food = uncurry translate (foodLoc game) $ color foodColor $ circleSolid 10
    foodColor = red

    snake = uncurry translate (snakeLoc game) $ rectangleSolid grid grid

drawing :: Picture
drawing = render initialState

main :: IO ()
main = display window background drawing
