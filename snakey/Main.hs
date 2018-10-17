module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe
import Debug.Trace


grid, width, height, offset, speed, stall :: Int
grid   = 20
width  = 50
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

elementOfMystery =
  do
    p <- newStdGen
    let d  = randomRs (0, 5 :: Int) p
    return d

--go to da place in a diff style
horizontalCrawl dir (x,y) (x1,y1)
    | x > x1    = 'a'
    | x < x1    = 'd'
    | otherwise = verticalCrawl dir (x,y) (x1,y1)

--go to da place
verticalCrawl dir (x,y) (x1,y1)
    | y > y1    = 's'
    | y < y1    = 'w'
    | otherwise = horizontalCrawl dir (x,y) (x1,y1)

--enemy snake's ai
evilWays dir myLoc meat foodLoc snake crawlType
    --go for the enemy snake
    | meat == 0 = eat dir myLoc foodLoc
    --go for the food
    | otherwise = eat dir myLoc (head snake)
    where
      eat dir me destination
        | crawlType == 0 = horizontalCrawl dir me destination
        | otherwise      = verticalCrawl dir me destination

--check if snake touches itself
touchySelf (h:t) = touchy h t
--check if snake touches enemy
touchEnemy me enemy = touchy me enemy
touchy myHead [] = False
touchy myHead (h:t)
    | myHead == h = True
    | otherwise = touchy myHead t

--check if snake goes out of bounds
touchBoundary (x, y)
    | x < minX + 1 || x > maxX - 1 || y < minY + 1 || y > maxY - 1 = True
    | otherwise    = False

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
  { foodLoc      :: [(Int, Int)]
  , snakeLoc     :: [(Int, Int)]
  , snakeDir     :: Char
  , evilSnake    :: [(Int, Int)]
  , roadToEvil   :: Char --basically snakeDir but fo evil snake but only for debugging purposes
  , choiceOfEvil :: [Int] -- randomized choice of which way the snake will go
  , meat         :: Int -- decide whether enemy snake chases snake or food
  , steps        :: Int -- stall the enemy snake every
  , menu         :: Int
  } deriving Show


-- | The starting state for the game
initialState :: SnakeGame
initialState = Game
  { foodLoc      = unsafePerformIO generateFood -- convert from IO [(Int, Int)] to [(Int, Int)]
  , snakeLoc     = [(0,0)]       -- center of the screen
  , snakeDir     = 'w'
  , evilSnake    = [((div width 3)*grid, (div height 3)*grid), ((div width 3)*grid, (div height 3)*grid-20) , ((div width 3)*grid, (div height 3)*grid-40)]
  , roadToEvil   = 'w' --basically snakeDir but fo evil snake
  , choiceOfEvil = unsafePerformIO elementOfMystery
  , meat         = 0 -- 0: chase food, 1: chase snake
  , steps        = 0
  , menu         = 0 -- 0: start game, 1: play game, 2: game over
  }

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game
  | (menu game) == 0 = pictures [startScreen, wallW, wallS, wallA, wallD]
  | (menu game) == 2 = pictures [deadScreen, wallW, wallS, wallA, wallD]
  | otherwise = pictures [food, snake, otherSnake, wallW, wallS, wallA, wallD]
  where
    --text
    deadScreen = (text "dead")
    --startScreen = translate -40 0 (text "press space to start")
    startScreen = unsafePerformIO (loadBMP "snake.bmp") :: Picture
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
  | key == KeySpace && (menu game) == 0 = initialState { menu = 1 } -- start game
  | key == KeyEnter && (menu game) == 2 = game { menu = 0 } -- go to init screen
  | key == KeyEsc = game { snakeLoc = [] } -- exit game
  | otherwise = game
-- Do nothing for all other events.
handleKeys _ game = game

update :: Float -> SnakeGame -> SnakeGame
update second game
  | (menu game) == 0 || (menu game) == 2 = game
  --snake is out of bounds
  | touchBoundary (head (snakeLoc game)) =
      trace ("snake out of bound")
      gameOver
  --snake touches itself
  | touchySelf (snakeLoc game) =
      trace ("snake ate itself")
      gameOver
  | touchEnemy (head (snakeLoc game)) (evilSnake game) =
      trace ("snake crashed into enemy")
      gameOver
  --snake eats food
  | head nextLoc == head (foodLoc game) = game
      { foodLoc      = decapitate (foodLoc game) --move food
      , snakeLoc     = (head nextLoc):(snakeLoc game) --add to snake
      , evilSnake    = pythonLoc
      , roadToEvil   = pythonDir
      , choiceOfEvil = decapitate (choiceOfEvil game)
      , steps        = (steps game) + 1
      }
  --enemy snake eats food
  | head pythonLoc == head (foodLoc game) = game
      { foodLoc      = decapitate (foodLoc game)
      , snakeLoc     = nextLoc
      , evilSnake    = pythonLoc
      , choiceOfEvil = decapitate (choiceOfEvil game)
      , steps        = (steps game) + 1
      }
  --stall enemy snake to make the game easier
  | (steps game) > stall = game
      { snakeLoc = nextLoc
      , steps    = 0
      }
  --nothing happens
  | otherwise = game
      { snakeLoc     = nextLoc
      , evilSnake    = pythonLoc
      , roadToEvil   = pythonDir
      , choiceOfEvil = decapitate (choiceOfEvil game)
      , steps        = (steps game) + 1
      }

  where
    nextLoc   = moveSnake (snakeDir game) (snakeLoc game)
    pythonDir = evilWays (roadToEvil game)(head (evilSnake game))(meat game) (head(foodLoc game)) (snakeLoc game) (head (choiceOfEvil game))
    pythonLoc = moveSnake pythonDir (evilSnake game)
    -- show game over screen
    gameOver = game
      { foodLoc = decapitate (foodLoc game)
      , menu = 2
      }
    -- return the rest of the list
    decapitate (h:t) = t

main :: IO ()
main = do   wall <- loadBMP "snake.bmp"
            play window background speed initialState render handleKeys update
