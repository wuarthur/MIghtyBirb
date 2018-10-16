module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe


grid, width, height, offset, speed :: Int
grid = 20
width = 50
height = 50
offset = 100
speed = 10  -- so we move 1 grid space per 1/speed seconds

window :: Display
window = InWindow "Snakey" (width * grid, height * grid) (offset, offset)

background :: Color
background = white

--generate a list of food locations
generateFood =
  do
    gx <- newStdGen
    gy <- newStdGen
    let xs = randomRs (-(div width 2)+1, (div width 2)-1) gx
    let ys = randomRs (-(div height 2)+1, (div height 2)-1) gy
    let lst = [ (x * grid, y * grid) | (x, y) <- (zip xs ys)]
    return lst

-- Describe the state of the game.
data SnakeGame = Game
  { foodLoc :: [(Int, Int)]
  , snakeLoc :: [(Int, Int)]
  , snakeDir :: Char
  } deriving Show


-- | The starting state for the game
initialState :: SnakeGame
initialState = Game
  { foodLoc = unsafePerformIO generateFood -- convert from IO [(Int, Int)] to [(Int, Int)]
  , snakeLoc =  [(0,0)]       -- center of the screen
  , snakeDir = 'w'
  }

-- return the rest of the food
moveFood (h:t) = t

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [food, snake]
  where
    food = uncurry translate loc $ color foodColor $ circleSolid 10
    loc = (fromIntegral(fst (head (foodLoc game))), fromIntegral(snd (head (foodLoc game))))
    foodColor = red
    snake = drawSnake snakeLocToFloat
    -- convert from Int to Float for gloss
    snakeLocToFloat = [ (fromIntegral(x), fromIntegral(y)) | (x,y) <- snakeLoc game]
    gridF = fromIntegral(grid)
    --actually draws the snake
    drawSnake list = (pictures (drawSnakeH list))
    drawSnakeH  [] = []
    drawSnakeH  (h:t) = (drawPart h):(drawSnakeH t)
    drawPart (x,y) = translate x y (rectangleSolid gridF gridF)

--snake render and move
moveSnake dir (h:t)
    | dir == 'w' =  (x, y+grid):(updateSnake x y t)
    | dir == 's' =  (x, y-grid):(updateSnake x y t)
    | dir == 'a' =  (x-grid, y):(updateSnake x y t)
    | dir == 'd' =  (x+grid, y):(updateSnake x y t)
    where
      x = fst h
      y = snd h
      updateSnake :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
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
  | head nextLoc == head (foodLoc game) =
    game { foodLoc = moveFood (foodLoc game) --move food
      , snakeLoc = (head nextLoc):(snakeLoc game) --add to snake
    }
  | otherwise = game { snakeLoc = nextLoc }
  where
    nextLoc = moveSnake (snakeDir game) (snakeLoc game)

main :: IO ()
main = play window background speed initialState render handleKeys update
