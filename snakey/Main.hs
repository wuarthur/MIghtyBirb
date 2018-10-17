module Main(main, SnakeGame, render, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import System.Random
import System.IO.Unsafe
import Debug.Trace


grid, width, height, offset, speed :: Int
grid = 20
width = 20
height = 20
offset = 100
speed = 3  -- so we move 1 grid space per 1/speed seconds

maxY = (div (grid * height) 2)
minY = -(div (grid * height) 2)
maxX = (div (grid*width) 2)
minX = - (div (grid*width) 2)



window :: Display
window = InWindow "Snakey" (width * grid, height * grid) (offset, offset)

background :: Color
background = white

--go to da place in a diff style
frontCrawl dir (x,y) (x1,y1)
    |x>x1 && dir/='a' ='a'
    |x<x1 && dir/='d' ='d'
    |y>y1 && dir/='s' ='s'
    |y<y1 && dir/='w' ='w'
    |otherwise = dir

--go to da place
backCrawl dir (x,y) (x1,y1)
    |y>y1 && dir/='s' ='s'
    |y<y1 && dir/='w' ='w'
    |x>x1 && dir/='a' ='a'
    |x<x1 && dir/='d' ='d'
    |otherwise = frontCrawl dir (x,y) (x1,y1)


veggetarian dir (x,y) (x1,y1)
    =backCrawl dir (x,y) (x1,y1)

-- calcDistance (x, y) (a,b)=  --bugg fix later if has energy
--     (abs (x-a)) + (abs (y-b))
--
--
-- getClosest xy snake=
--     getClosestH xy snake 999999
--
-- getClosestH xy [] n cord = cord
-- getClosestH xy (h:t) n cord
--     | new< n = getClosestH xy t new h
--     |otherwise = getClosestH xy t n cord
--     where
--     new = calcDistance xy h



nomnom dir myLoc snake =
    backCrawl dir myLoc (head snake)

evilWays dir myLoc meat foodLoc snake
    |meat == 0 = veggetarian dir myLoc foodLoc
    |otherwise = nomnom dir myLoc snake

touchySelf (h:t) = touchy h t
touchy head [] = False
touchy head (h:t)
    | head == h = True
    | otherwise = touchy head t

touchBoundary (x, y)
    |x<minX = trace ("input x: " ++ show x)True
    |x>maxX = trace ("input x: " ++ show x)True
    |y<minY = trace ("input x: " ++ show y)True
    |y>maxY = trace ("input x: " ++ show y)True
    |otherwise = trace ("input: " ++ show x ++ " ,  " ++ show y) False

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
  , evilSnake :: [(Int, Int)]
  , roadToEvil :: Char --basically snakeDir but fo evil snake but only for debugging purposes
  , meat :: Int
  , steps ::Int
  } deriving Show


-- | The starting state for the game
initialState :: SnakeGame
initialState = Game
  { foodLoc = unsafePerformIO generateFood -- convert from IO [(Int, Int)] to [(Int, Int)]
  , snakeLoc =  [(0,0)]       -- center of the screen
  , snakeDir = 'w'
  , evilSnake = [(200, 30)]
  , roadToEvil = 'w' --basically snakeDir but fo evil snake
  , meat = 2 -- 0: chase food,
  , steps = 0
  }

-- return the rest of the food
moveFood (h:t) = t

-- | Convert a game state into a picture.
render :: SnakeGame  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures [food, snake, otherSnake]
  where
    food = uncurry translate loc $ color foodColor $ circleSolid 10
    loc = (fromIntegral(fst (head (foodLoc game))), fromIntegral(snd (head (foodLoc game))))
    foodColor = red
    snake = drawSnake snakeLocToFloat
    otherSnake = drawESnake [ (fromIntegral(x), fromIntegral(y)) | (x,y) <- evilSnake game]
    -- convert from Int to Float for gloss
    snakeLocToFloat = [ (fromIntegral(x), fromIntegral(y)) | (x,y) <- snakeLoc game]
    gridF = fromIntegral(grid)
    --actually draws the snake
    drawSnake list = (pictures (drawSnakeH list))
    drawSnakeH  [] = []
    drawSnakeH  (h:t) = (drawPart h):(drawSnakeH t)
    drawPart (x,y) = translate x y (rectangleSolid gridF gridF)
    --draws the evil snakey
    drawESnake list = (pictures (drawESnakeH list))
    drawESnakeH  [] = []
    drawESnakeH  (h:t) = (drawEvilPart h):(drawESnakeH t)
    drawEvilPart (x,y) = color green (translate x y (rectangleSolid gridF gridF))

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
  | touchBoundary (head (snakeLoc game)) == True =
     trace ("snake out of bound: " )
     game {
          snakeLoc = [] ----todo make end scene, rn it jsut dissapepars
           }
  | touchySelf (snakeLoc game) ==True =
       trace ("snake ate itself: " )
       game {
            snakeLoc = [] ----todo make end scene, rn it jsut dissapepars
             }

  | (steps game) >3 =game {
     snakeLoc = nextLoc
     ,steps = 0}
  | head nextLoc == head (foodLoc game) =
    game { foodLoc = moveFood (foodLoc game) --move food
      , snakeLoc = (head nextLoc):(snakeLoc game) --add to snake
      ,evilSnake = pythonLoc
      ,roadToEvil = pythonDir
      ,steps = (steps game) + 1
    }
  | otherwise = game {
    snakeLoc = nextLoc,
    evilSnake = pythonLoc
    ,roadToEvil = pythonDir
    ,steps = (steps game) + 1}

  where
    nextLoc = moveSnake (snakeDir game) (snakeLoc game)
    pythonDir = (evilWays (roadToEvil game)(head (evilSnake game))(meat game) (head(foodLoc game)) (snakeLoc game)) --todo update this
    pythonLoc = moveSnake pythonDir (evilSnake game)

main :: IO ()
main = play window background speed initialState render handleKeys update
