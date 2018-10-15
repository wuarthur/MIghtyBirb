module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort


grid :: Float
grid = 20

width, height, offset, speed :: Int
width = 50
height = 50
offset = 100
speed = 10  -- so we move 1 grid space per 1/speed seconds

window :: Display
window = InWindow "Snakey" (width * 20, height * 20) (offset, offset)

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
  , snakeLoc =  [(0,0)]       -- center of the screen
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
    drawSnakeH  [] = []
    drawSnakeH  (h:t) = (drawPart h):(drawSnakeH t)
    --actually draws the snake
    drawSnake list = (pictures (drawSnakeH list))
    drawPart (x,y) = translate x y (rectangleSolid grid grid)


--food
moveFood :: SnakeGame -> (Float, Float)
moveFood game = (0,0) --randomly generate food

--snake render and move
moveSnake dir (h:t)
    | dir == 'w' =  (x, y+grid):(updateSnake x y t)
    | dir == 's' =  (x, y-grid):(updateSnake x y t)
    | dir == 'a' =  (x-grid, y):(updateSnake x y t)
    | dir == 'd' =  (x+grid, y):(updateSnake x y t)
    where
      x = fst h
      y = snd h
      --updateSnake :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
      updateSnake _ _ [] = []
      updateSnake x y (h:t)= (x,y):(updateSnake (fst h) (snd h) t)


-- | Respond to key events.
handleKeys (EventKey (Char key) _ _ _) game
  | key == 'w' && (snakeDir game) /= 's' = game { snakeDir = key }
  | key == 's' && (snakeDir game) /= 'w' = game { snakeDir = key }
  | key == 'a' && (snakeDir game) /= 'd' = game { snakeDir = key }
  | key == 'd' && (snakeDir game) /= 'a' = game { snakeDir = key }
  | otherwise = game
-- Do nothing for all other events.
handleKeys _ game = game

update :: Float -> SnakeGame -> SnakeGame
update second game
  --if snake eats food (snake head = food location)
  | head (moveSnake (snakeDir game) (snakeLoc game)) == (foodLoc game) =
    game { foodLoc = moveFood game --move food
      , snakeLoc = (foodLoc game):(snakeLoc game) --add to snake
    }
  | otherwise =
    game {
      snakeLoc = (moveSnake (snakeDir game) (snakeLoc game))
    }

main :: IO ()
main = play window background speed initialState render handleKeys update
