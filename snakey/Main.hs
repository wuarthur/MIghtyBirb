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

drawing :: Picture
drawing = pictures
  [ food
  , snake
  ]

main :: IO ()
main = display window background drawing
