module Main(main) where

import Graphics.Gloss

width, height, offset :: Int
width = 300
height = 300
offset = 100


grid :: Float
grid = 20

window :: Display
window = InWindow "Snakey" (width, height) (offset, offset)

background :: Color
background = white

food :: Picture
food = translate startX startY $ color foodColor $ circleSolid (grid / 2)
  where
    foodColor = red
    startX = -20
    startY = -100

snake :: Picture
snake = rectangleSolid grid grid


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
drawing = pictures
  (drawSnake newslake)

main :: IO ()
main = display window background drawing


