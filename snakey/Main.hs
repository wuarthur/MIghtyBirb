module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe
import Debug.Trace


grid, width, height, offset, speed, stall :: Int
grid   = 20
width  = 20
height = 20
offset = 100
speed  = 5  -- so we move 1 grid space per 1/speed seconds
stall  = 1  -- steps enemy snake can take before it waits a tick

maxY = grid * (div height 2)
minY = grid * (-(div height 2))
maxX = grid * (div width 2)
minX = grid * (-(div width 2))



window :: Display
window = InWindow "Snakey" (width * grid, height * grid) (offset, offset)

background :: Color
background = white

foodColor, sHeadColor, sColor, eColor, wallColor :: Color
foodColor  = red
sHeadColor = blue
sColor     = black
eColor     = green
wallColor  = (makeColor 0.5 0.5 0.5 1)

--go to da place in a diff style
horizontalCrawl dir (x,y) (x1,y1)
    | x > x1 && dir /= 'a' = 'a'
    | x < x1 && dir /= 'd' = 'd'
    | y > y1 && dir /= 's' = 's'
    | y < y1 && dir /= 'w' = 'w'
    | otherwise            = dir

--go to da place
verticalCrawl dir (x,y) (x1,y1)
    | y > y1 && dir /= 's' = 's'
    | y < y1 && dir /= 'w' = 'w'
    | x > x1 && dir /= 'a' = 'a'
    | x < x1 && dir /= 'd' = 'd'
    | otherwise            = horizontalCrawl dir (x,y) (x1,y1)

--enemy snake's ai
evilWays dir myLoc meat foodLoc snake
    |meat == 0 = vegetarian dir myLoc foodLoc
    |otherwise = nomnom dir myLoc snake
    where
      --go for the enemy snake
      nomnom dir myLoc snake       = verticalCrawl dir myLoc (head snake)
      --go for the food
      vegetarian dir (x,y) (x1,y1) = verticalCrawl dir (x,y) (x1,y1)

--check if snake touches itself
touchySelf (h:t) = touchy h t
touchy head [] = False
touchy head (h:t)
    | head == h = True
    | otherwise = touchy head t

--check if snake goes out of bounds
touchBoundary (x, y)
    | x < minX + 1 = trace ("input x: " ++ show x) True
    | x > maxX - 1 = trace ("input x: " ++ show x) True
    | y < minY + 1 = trace ("input x: " ++ show y) True
    | y > maxY - 1 = trace ("input x: " ++ show y) True
    | otherwise    = trace ("input: " ++ show x ++ " ,  " ++ show y) False

--generate a list of food locations
generateFood =
  do
    gx <- newStdGen
    gy <- newStdGen
    let xs  = randomRs (-(div width 2)+1 , (div width 2)-1 ) gx
    let ys  = randomRs (-(div height 2)+1, (div height 2)-1) gy
    let lst = [ (x * grid, y * grid) | (x, y) <- (zip xs ys)]
    return lst

-- Describe the state of the game.
data SnakeGame = Game
  { foodLoc    :: [(Int, Int)]
  , snakeLoc   :: [(Int, Int)]
  , snakeDir   :: Char
  , evilSnake  :: [(Int, Int)]
  , roadToEvil :: Char --basically snakeDir but fo evil snake but only for debugging purposes
  , meat       :: Int -- decide whether enemy snake chases snake or food
  , steps      :: Int -- stall the enemy snake every
  , menu       :: Bool --menu is open
  } deriving Show


-- | The starting state for the game
initialState :: SnakeGame
initialState = Game
  { foodLoc    = unsafePerformIO generateFood -- convert from IO [(Int, Int)] to [(Int, Int)]
  , snakeLoc   = [(0,0)]       -- center of the screen
  , snakeDir   = 'w'
  , evilSnake  = [((div width 3)*grid, (div height 3)*grid), ((div width 3)*grid, (div height 3)*grid-20) , ((div width 3)*grid, (div height 3)*grid-40)]
  , roadToEvil = 'w' --basically snakeDir but fo evil snake
  , meat       = 0 -- 0: chase food, 1: chase snake
  , steps      = 0
  , menu       = True
  }

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [food, snake, otherSnake, wallW, wallS, wallA, wallD]
  where
    --food
    food = uncurry translate loc $ color foodColor $ circleSolid 10
    loc = (fromIntegral(fst (head (foodLoc game))), fromIntegral(snd (head (foodLoc game))))
    --snake
    snake = drawSnake (snakeLocToFloat snakeLoc) False
    otherSnake = drawSnake (snakeLocToFloat evilSnake) True
    -- convert from Int to Float for gloss
    snakeLocToFloat s = [ (fromIntegral(x), fromIntegral(y)) | (x,y) <- s game]
    gridF = fromIntegral(grid)
    --actually draws the snake
    drawSnake lst enemySnake
      | enemySnake = pictures (drawSnakeH lst eColor)
      | otherwise  = pictures ((drawPart (head lst) sHeadColor):(drawSnakeH (tail lst) sColor))
      where
        drawSnakeH [] _    = []
        drawSnakeH (h:t) c = (drawPart h c):(drawSnakeH t c)
        --picture part
        drawPart (x,y) c   = color c $ translate x y (rectangleSolid gridF gridF)
    --wall
    wallW = color wallColor $ translate 0 (fromIntegral maxY) (rectangleSolid wallWidth gridF)
    wallS = color wallColor $ translate 0 (fromIntegral minY) (rectangleSolid wallWidth gridF)
    wallA = color wallColor $ translate (fromIntegral maxX) 0 (rectangleSolid gridF wallHeight)
    wallD = color wallColor $ translate (fromIntegral minX) 0 (rectangleSolid gridF wallHeight)
    wallWidth = fromIntegral(width * grid)
    wallHeight = fromIntegral(height * grid)

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
      updateSnake _ _ []    = []
      updateSnake x y (h:t) = (x,y):(updateSnake (fst h) (snd h) t)


-- | Respond to key events.
handleKeys (EventKey (Char key) _ _ _) game
  | key == 'w' && (snakeDir game) /= 's' = game { snakeDir = key }
  | key == 's' && (snakeDir game) /= 'w' = game { snakeDir = key }
  | key == 'a' && (snakeDir game) /= 'd' = game { snakeDir = key }
  | key == 'd' && (snakeDir game) /= 'a' = game { snakeDir = key }
  | otherwise = game
handleKeys (EventKey (SpecialKey key) _ _ _) game
  | key == KeyUp    && (snakeDir game) /= 's' = game { snakeDir = 'w' }
  | key == KeyDown  && (snakeDir game) /= 'w' = game { snakeDir = 's' }
  | key == KeyLeft  && (snakeDir game) /= 'd' = game { snakeDir = 'a' }
  | key == KeyRight && (snakeDir game) /= 'a' = game { snakeDir = 'd' }
  | key == KeySpace = game { menu = False }
  | otherwise = game
-- Do nothing for all other events.
handleKeys _ game = game

update :: Float -> SnakeGame -> SnakeGame
update second game
--  | menu game = game --TODO make this display a menu
  --snake is out of bounds
  | touchBoundary (head (snakeLoc game)) == True =
     trace ("snake out of bound: " )
     game
      { snakeLoc = [] ----todo make end scene, rn it jsut dissapepars
      }
  --snake touches itself
  | touchySelf (snakeLoc game) ==True =
      trace ("snake ate itself: " )
      game
       { snakeLoc = [] ----todo make end scene, rn it jsut dissapepars
       }
  --snake eats food
  | head nextLoc == head (foodLoc game) = game
     { foodLoc    = moveFood (foodLoc game) --move food
     , snakeLoc   = (head nextLoc):(snakeLoc game) --add to snake
     , evilSnake  = pythonLoc
     , roadToEvil = pythonDir
     , steps      = (steps game) + 1
     }
  --enemy snake eats food
  | head pythonLoc == head (foodLoc game) = game
     { foodLoc   = moveFood (foodLoc game)
     , snakeLoc  = nextLoc
     , evilSnake = pythonLoc
     , steps     = (steps game) + 1
     }
  --stall enemy snake to make the game easier
  | (steps game) > stall = game
     { snakeLoc = nextLoc
     , steps    = 0
     }
  --nothing happens
  | otherwise = game
    { snakeLoc   = nextLoc
    , evilSnake  = pythonLoc
    , roadToEvil = pythonDir
    , steps      = (steps game) + 1
    }

  where
    nextLoc   = moveSnake (snakeDir game) (snakeLoc game)
    pythonDir = (evilWays (roadToEvil game)(head (evilSnake game))(meat game) (head(foodLoc game)) (snakeLoc game)) --todo update this
    pythonLoc = moveSnake pythonDir (evilSnake game)
    -- return the rest of the food
    moveFood (h:t) = t

main :: IO ()
main = do   wall <- loadBMP "snake.bmp"
            play window background speed initialState render handleKeys update
