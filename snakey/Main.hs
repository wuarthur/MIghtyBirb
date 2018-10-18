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
height = 30
offset = 100
speed  = 5  -- so we move 1 grid space per 1/speed seconds
stall  = 1  -- enemy snake pauses a step every stall steps (at stall = 1, the enemy snake runs at half the speed of the player)

maxY, minY, maxX, minX :: Int
maxY = grid * (div height 2)
minY = grid * (-(div height 2))
maxX = grid * (div width 2)
minX = grid * (-(div width 2))



window :: Display
window = InWindow "Snakey" (width * grid, height * grid) (offset, offset)

background, wallColor :: Color
background = white
wallColor  = (makeColor 0.5 0.5 0.5 1)

foodColor, sHeadColor, sColor, eColor :: Color
foodColor  = (makeColor 0.882 0.137 0.294 1)
sHeadColor = (makeColor 0.286 0.38 0.733 1)
sColor     = (makeColor 0.11 0.149 0.294 1)
eColor     = (makeColor 0.38 0.733 0.286 1)



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
  where
    --generate a list of food locations
    generateFood =
      do
        gx <- newStdGen
        gy <- newStdGen
        let xs  = randomRs (-(div width 2)+1 , (div width 2)-1 ) gx
        let ys  = randomRs (-(div height 2)+1, (div height 2)-1) gy
        let lst = [ (x * grid, y * grid) | (x, y) <- (zip xs ys)]
        return lst
    --randomly generate the order of turns the enemy snake makes
    elementOfMystery =
      do
        p <- newStdGen
        let d  = randomRs (0, 5 :: Int) p
        return d

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game
  --render start screen
  | (menu game) == 0 = pictures [startScreen, wallW, wallS, wallA, wallD]
  --render game over screen
  | (menu game) == 2 = pictures [deadScreen, wallW, wallS, wallA, wallD]
  | otherwise = pictures [food, snake, otherSnake, wallW, wallS, wallA, wallD]
  where
    --assets for start screen
    startScreen = unsafePerformIO (loadBMP "snake.bmp") :: Picture
    --assets for game over screen
    deadScreen = unsafePerformIO (loadBMP "gameover.bmp") :: Picture
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
    drawSnake lst isEnemySnake
      | isEnemySnake = pictures (drawSnakeH lst eColor)
      | otherwise    = pictures ((drawPart (head lst) sHeadColor):(drawSnakeH (tail lst) sColor))
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



-- Respond to key events.
--char keys
handleKeys (EventKey (Char key) _ _ _) game
  | key == 'w' && (snakeDir game) /= 's' = game { snakeDir = key }
  | key == 's' && (snakeDir game) /= 'w' = game { snakeDir = key }
  | key == 'a' && (snakeDir game) /= 'd' = game { snakeDir = key }
  | key == 'd' && (snakeDir game) /= 'a' = game { snakeDir = key }
  | (menu game) == 0 && key == '1' = game { meat = 0 } --enemy chases food
  | (menu game) == 0 && key == '2' = game { meat = 1 } --enemy chases you
  | otherwise = game
--special keys
handleKeys (EventKey (SpecialKey key) _ _ _) game
  | key == KeyUp    && (snakeDir game) /= 's' = game { snakeDir = 'w' }
  | key == KeyDown  && (snakeDir game) /= 'w' = game { snakeDir = 's' }
  | key == KeyLeft  && (snakeDir game) /= 'd' = game { snakeDir = 'a' }
  | key == KeyRight && (snakeDir game) /= 'a' = game { snakeDir = 'd' }
  | key == KeySpace && (menu game) == 0 = initialState
      { meat = (meat game)
      , menu = 1
      } -- start game
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
  --snake touches enemy
  | touchEnemy (head (snakeLoc game)) (evilSnake game) =
      trace ("snake crashed into enemy")
      gameOver
  --snake eats food
  | head nextLoc == head (foodLoc game) = continue
      { foodLoc  = tail (foodLoc game) --move food
      , snakeLoc = (head nextLoc):(snakeLoc game) --add to snake
      }
  --enemy snake eats food
  | head pythonLoc == head (foodLoc game) = continue
      { foodLoc  = tail (foodLoc game)
      }
  --stall enemy snake to make the game easier
  | (steps game) > stall = game
      { snakeLoc = nextLoc
      , steps    = 0
      }
  --nothing happens
  | otherwise = continue
  where
    -- show game over screen
    gameOver = game
      { foodLoc = tail (foodLoc game)
      , menu = 2
      }
    -- normal case, move snakes forward
    continue = game
      { snakeLoc     = nextLoc
      , evilSnake    = pythonLoc
      , roadToEvil   = pythonDir
      , choiceOfEvil = tail (choiceOfEvil game)
      , steps        = (steps game) + 1
      }

    --Game over check
    --check if snake touches itself
    touchySelf (h:t) = touchy h t
    --check if snake touches enemy
    touchEnemy me enemy = touchy me enemy

    --check if snake touches any element in the list
    touchy myHead [] = False
    touchy myHead (h:t)
        | myHead == h = True
        | otherwise = touchy myHead t

    --check if snake goes out of bounds
    touchBoundary (x, y)
        | x < minX + 1 || x > maxX - 1 || y < minY + 1 || y > maxY - 1 = True
        | otherwise    = False


    --snake render and move
    nextLoc   = moveSnake (snakeDir game) (snakeLoc game)
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


    --enemy snake's ai
    pythonLoc = moveSnake pythonDir (evilSnake game)
    pythonDir = evilWays (roadToEvil game)(head (evilSnake game))(meat game) (head(foodLoc game)) (snakeLoc game) (head (choiceOfEvil game))
    evilWays dir myLoc meat foodLoc snake crawlType
        --go for the enemy snake
        | meat == 0 = eat dir myLoc foodLoc
        --go for the food
        | otherwise = eat dir myLoc (head snake)
        where
          eat dir me destination
            | crawlType == 0 = horizontalCrawl dir me destination
            | otherwise      = verticalCrawl dir me destination
          --enemy snake moves horizontally towards the target
          horizontalCrawl dir (x,y) (x1,y1)
              | x > x1    = 'a'
              | x < x1    = 'd'
              --same horizontal coordinate as the target, must move vertically
              | otherwise = verticalCrawl dir (x,y) (x1,y1)

          --enemy snake moves vertically towards the target
          verticalCrawl dir (x,y) (x1,y1)
              | y > y1    = 's'
              | y < y1    = 'w'
              --same vertical coordinate as the target, must move horizontally
              | otherwise = horizontalCrawl dir (x,y) (x1,y1)


main :: IO ()
main = play window background speed initialState render handleKeys update
