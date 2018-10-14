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


realSnake = [(0,0), (20,0), (40,0), (60,0)]

updateSnake :: Float -> Float -> [(Float, Float)] -> [(Float, Float)]
updateSnake _ _ [] = []
updateSnake x y (h:t)=
    do
        newX<-fst h
        newY<-snd h
        return ((x,y):(updateSnake newX newY t))

moveSnake dir (h:t)
    | dir == "UP" =  ((fst h)+grid, (snd h)+grid):(updateSnake (fst h) (snd h) t)

newslake = moveSnake "UP" realSnake

drawSnake [] = []
drawSnake (h:t) = (drawPart h):(drawSnake t)

drawPart (x,y) = translate x y (rectangleSolid grid grid)

drawing :: Picture
drawing = render initialState


main :: IO ()
main = display window background drawing


