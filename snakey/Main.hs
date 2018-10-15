module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

grid :: Float
grid = 20

width, height, offset, speed :: Int
width = 20 * 50 --change the 20 to grid TODO
height = 20 * 50
offset = 100
speed = 10  -- so we move 1 grid space per 1/speed seconds

window :: Display
window = InWindow "Snakey" (width, height) (offset, offset)

background :: Color
background = white


-- Describe the state of the game.
data SnakeGame = Game
  { foodLoc :: (Float, Float)
  , snakeLoc :: [(Float, Float)]
  , snakeDir :: Char
  } deriving Show


-- | The starting state for the game
initialState :: SnakeGame
initialState = Game
  { foodLoc = (-grid, grid*5)
  , snakeLoc =  [(0,0), (20,0), (40,0), (60,0)]       -- center of the screen
  , snakeDir = 'w'
  }

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [food, snake]
  where
    food = uncurry translate (foodLoc game) $ color foodColor $ circleSolid 10
    foodColor = red
    snake = drawSnake (snakeLoc game)


--snake render and move
--updateSnake :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
updateSnake _ _ [] = []
updateSnake x y (h:t)= (x,y):(updateSnake (fst h) (snd h) t)

--keep the state somewhere of the snake to make sure it cannot move backward
moveSnake dir (h:t)
    | dir == 'w' =  (x, y+grid):(updateSnake x y t)
    | dir == 's' =  (x, y-grid):(updateSnake x y t)
    | dir == 'a' =  (x-grid, y):(updateSnake x y t)
    | dir == 'd' =  (x+grid, y):(updateSnake x y t)
    where
      x = fst h
      y = snd h

drawSnakeH  [] = []
drawSnakeH  (h:t) = (drawPart h):(drawSnakeH t)
--actually draws the snake
drawSnake list = (pictures (drawSnakeH list))
drawPart (x,y) = translate x y (rectangleSolid grid grid)
--snake render and move


-- | Respond to key events.
-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 'w') _ _ _) game
   |(snakeDir game) == 's' = game
   |otherwise =
    game { foodLoc = (foodLoc game)
   , snakeLoc = (moveSnake 'w' (snakeLoc game))        -- center of the screen
   , snakeDir = 'w'
   }

handleKeys (EventKey (Char 's') _ _ _) game
   |(snakeDir game) == 'w' = game
   |otherwise =
   game { foodLoc = (foodLoc game)
   , snakeLoc = (moveSnake 's' (snakeLoc game))        -- center of the screen
   , snakeDir = 's'
   }

handleKeys (EventKey (Char 'd') _ _ _) game
    |(snakeDir game) == 'a' = game
    |otherwise =
    game { foodLoc = (foodLoc game)
    , snakeLoc = (moveSnake 'd' (snakeLoc game))        -- center of the screen
    , snakeDir = 'd'
}

handleKeys (EventKey (Char 'a') _ _ _) game
   |(snakeDir game) == 'd' = game
   |otherwise =
    game { foodLoc = (foodLoc game)
    , snakeLoc = (moveSnake 'a' (snakeLoc game))        -- center of the screen
    , snakeDir = 'a'
 }
-- Do nothing for all other events.
handleKeys _ game = game

update :: Float -> SnakeGame -> SnakeGame
update second game =
    game { foodLoc = (foodLoc game)
        , snakeLoc = (moveSnake (snakeDir game) (snakeLoc game))         -- center of the screen
        , snakeDir = (snakeDir game)
     }

--(pictures (drawSnake realSnake))
main :: IO ()
main = play window background speed initialState render handleKeys update
