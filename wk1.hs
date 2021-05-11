-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld
{- There are 3 exercises in this file.
To execute the code from exercises 2 or 3, 
simply change main (line 9) accordingly -}

main :: IO ()
main = exercise1

-- Exercise 1
{- Animate a traffic light -}

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-3) (solidCircle 1))
midCircle c = colored c (translated 0 (0) (solidCircle 1))
topCircle c = colored c (translated 0 3 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

trafficLight :: Int  -> Picture
trafficLight 1 = botCircle green & midCircle black & topCircle black & frame
trafficLight 2 = botCircle black & midCircle (light(orange)) & topCircle black & frame
trafficLight 3 = botCircle black & midCircle black & topCircle red  & frame
trafficLight 4 = botCircle black & midCircle (light(orange)) & topCircle red & frame

{- Light 1 (green) and 3 (red) are long, 
and light 2 (amber) and 4 (red and amber) are short -}
trafficController :: Double -> Picture
trafficController t
  | res <= 1             = trafficLight 1
  | res == 2             = trafficLight 2
  | res == 3 || res == 4 = trafficLight 3
  | otherwise            = trafficLight 4
  where res = round t `mod` 6

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2
{- Create an animation that looks like the dire tree 8 initially,
and then grows blossoms at the end of each twig within 10 seconds.
After 10 seconds, the tree should be in full bloom and the 
animation should stop. -}
    
-- Firstly, we define a petal of a flower
petal :: Double -> Picture
petal d = rotated (pi/8) (polyline [(0,0), (0,d)]) & rotated (-pi/8) (polyline [(0,0), (0,d)]) & arc (3*pi/8) (5*pi/8) d

-- Then we define a flower. We can pass its colour and size as a parameter
flower :: Color -> Double -> Picture
flower c d = colored c (petal d & (rotated (pi/2) (petal d)) & (rotated (pi) (petal d)) & (rotated (3*pi/2) (petal d)))

-- Then we define an instance of `bloom`: could be true (flower) or false (not a flower yet)
bloom :: Bool -> Color -> Double -> Picture
bloom b c d
  |b         = flower c d
  |otherwise = blank
-- (I wanted to write a version of bloom where you can pass the type of flower as a parameter, in case we happen to have more than one flower function. I tried doing it but got a bit lost with the types. I'll revisit this later.)

-- Finally, we can define our tree
-- It takes as input a Boolean that tells you whether the tree has already flowered or not,
-- as well as a Color parameter indicating the color of the flowers, a Double indicating the size of the flowers,
-- and an Integer that specifies the size of our tree
tree :: Bool -> Color -> Double -> Integer -> Picture
tree _ _ _ 0 = blank
tree f c d n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree f c d (n-1)) & rotated (- pi/10) (tree f c d (n-1)) &
  bloom f c d) 

-- we can make our tree blossom gradually by gradually enlarging and darkening its flower buds
treeBlossoming :: Double -> Picture
treeBlossoming t
  |t < 1 = (tree False pink 0 8) -- bare tree
  |t > 1 && t <= 2 = tree True (light(light(light(red)))) 0.15 8
  |t > 2 && t <= 4 = tree True (light(light(red))) 0.2 8
  |t > 4 && t <= 6 = tree True (light(red)) 0.25 8
  |t > 6 && t <= 8 = tree True red 0.3 8
  |t > 8 && t <= 10 = tree True red 0.35 8
  |t > 10 = tree True (dark(red)) 0.4 8

exercise2 :: IO ()
exercise2 = animationOf treeBlossoming


-- Exercise 3

{-  Draw a sokoban level -}

-- We start by defining each block
wall, ground, storage, box :: Picture

-- a wall unit is a brick
wall    = polygon [(-0.5,-0.5), (-0.5,-1/6), (0,-1/6), (0,-0.5)] &
          polygon [(0,-0.5), (0,-1/6), (0.5,-1/6), (0.5, -0.5)] &
          polygon [(-0.5,-1/6), (-0.5, 1/6), (-1/4,1/6), (-1/4, -1/6)] &
          polygon[(-1/4, -1/6), (-1/4, 1/6), (1/4, 1/6), (1/4, -1/6)] &
          polygon [(1/4, -1/6), (1/4, 1/6), (0.5, 1/6), (0.5, -1/6)] &
          polygon [(-0.5,1/6), (-0.5, 0.5), (0, 0.5), (0,1/6)] &
          polygon [(0,1/6), (0, 0.5), (0.5, 0.5), (0.5, 1/6)] &
          colored (light(brown)) (solidRectangle 1 1)
          
-- ground is grey with small black spots
ground  = translated (-1/6) (-1/4) (solidCircle 0.02) &
          translated (0.25) (-0.35) (solidCircle 0.02) &
          translated (0.3) (0.4) (solidCircle 0.02) &
          translated (-0.45) (0.1) (solidCircle 0.02) &
          translated (0.1) (0.05) (solidCircle 0.02) &
          colored (light(grey)) (solidRectangle 1 1)    
          
-- storage spaces are units of ground with a translucent red circle 
storage = colored (translucent(red)) (solidCircle 0.3) &
          ground

-- and boxes are just brown (I ran out of ideas...)
box     = rectangle 1 1 &
          rectangle 0.6 0.6 &
          colored brown (solidRectangle 0.6 0.6) &
          colored (dark(brown)) (solidRectangle 1 1)

-- Function to draw the corresponding tile
drawTile :: Integer -> Picture
drawTile k
  |k == 0 = blank
  |k == 1 = wall
  |k == 2 = ground
  |k == 3 = storage
  |k == 4 = box

-- Maze map (provided to us): indicates what type of block is in each grid cell 
maze :: Integer -> Integer -> Integer 
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

-- We can now draw out the board based on the map provided
-- Firstly, we define a function to recursively draw out the rows.
-- We start at row 0 and move in both directions so that our resulting figure is centered
draw_row :: Integer -> Picture
draw_row y
  |y == 0 = draw_cell 0 0 & (translated 0 1 (draw_row (y+1))) & (translated 0 (-1) (draw_row (y-1)))
  |y == 10 || y == (-10) = draw_cell 0 y
  |y < 0 = draw_cell 0 y & (translated 0 (-1) (draw_row (y-1)))
  |y > 0 = draw_cell 0 y & (translated 0 1 (draw_row (y+1)))
  
-- The row function above calls this ``draw_cell`` function, which draws all cells in a row,
-- starting in column 0 (same logic as draw_row)
draw_cell :: Integer -> Integer -> Picture
draw_cell x y
  |x == 0 = (drawTile(maze x y)) & (translated 1 0 (draw_cell (x + 1) y)) & (translated (-1) 0 (draw_cell (x - 1) y))
  |x == 10 || x == (-10) = drawTile(maze x y)
  |x > 0 = (drawTile(maze x y)) & (translated 1 0 (draw_cell (x + 1) y))
  |x < 0 = (drawTile(maze x y)) & (translated (-1) 0 (draw_cell (x - 1) y)) 

-- ...and we're finally ready to draw our picture :)
pictureOfMaze :: Picture

pictureOfMaze = (draw_row 0)

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze
